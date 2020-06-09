library(tidyverse)
library(lubridate)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(animation)

library(RMariaDB)
con <- dbConnect(RMariaDB::MariaDB(), 
                 default.file = file.path(getwd(), ".my.cnf"),
                 group = "my-db")

df_eCommerce <- dbFetch(dbSendQuery(con, "SELECT * FROM retail_uk_ecom_ecommerce"))
df_retail <- dbFetch(dbSendQuery(con, "SELECT * FROM retail_uk_ecom_retail_mainline"))


# Table & Scratch Pad
df_eCommerce %>%
  filter(net_cost_gbp > 0) %>%
  group_by(transaction_number) %>%
  summarise(
    total_net = sum(net_cost_gbp)
  ) %>% pull(total_net) %>% summary()

df_retail %>%
  filter(net_cost_gbp > 0) %>%
  group_by(transaction_number) %>%
  summarise(
    total_net = sum(net_cost_gbp)
  ) %>% pull(total_net) %>% summary()

eCommerceTop <- df_eCommerce %>%
  select(transaction_number, sales_month, SKU) %>%
  # remove SKUs related to delivery
  filter(!(SKU %in% c(411554, 411509, 431934, 437417, 411547,
                      431903, 431910, 411578, 411592, 411523,
                      411516, 437806, 610551, 432016, 437813,
                      431941, 477949, 610568, 433464, 431958, 800112, 411530,
                      866088 # gift with purchase
  ))) %>% 
  group_by(sales_month, SKU) %>% 
  summarise(
    SKU_appearance = length(SKU)
  ) %>% 
  group_by(sales_month) %>% 
  mutate(rank = dense_rank(desc(SKU_appearance))) %>% 
  ungroup() %>% 
  arrange(sales_month, desc(SKU_appearance)) %>% 
  filter(rank <= 10)
  # filter(sales_month == as_date("2019-09-01"))

retailTop <- df_retail %>%
  select(transaction_number, sales_month, SKU) %>%
  # remove SKUs related to delivery
  filter(!(SKU %in% c(411554, 411509, 431934, 437417, 411547,
                      431903, 431910, 411578, 411592, 411523,
                      411516, 437806, 610551, 432016, 437813,
                      431941, 477949, 610568, 433464, 431958, 800112, 411530,
                      866088 # gift with purchase
  ))) %>% 
  group_by(sales_month, SKU) %>% 
  summarise(
    SKU_appearance = length(SKU)
  ) %>% 
  group_by(sales_month) %>% 
  mutate(rank = dense_rank(desc(SKU_appearance))) %>% 
  ungroup() %>% 
  arrange(sales_month, desc(SKU_appearance)) %>% 
  filter(sales_month == as_date("2019-09-01"))

eCommerceTop %>% full_join(retailTop, by = c("sales_month", "SKU"), suffix = c("_eCommerce", "_retail")) %>% View()


# RFM Score comparison
df_eCommerce %>%
  group_by(customer_account) %>%
  summarise(
    mean = mean(RFM_Score),
    median = median(RFM_Score)
) %>% ungroup() %>% summarise(mean=mean(mean, na.rm = T), median=median(median, na.rm = T))
df_retail %>%
  group_by(customer_account) %>%
  summarise(
    mean = mean(RFM_Score),
    median = median(RFM_Score)
  ) %>% ungroup() %>% summarise(mean=mean(mean, na.rm = T), median=median(median, na.rm = T))


make_retail_basket <- function(n, date_string) {
  df_eCommerce_top_monthly <- 
    df_eCommerce %>%
    select(transaction_number, sales_month, SKU) %>%
    # remove SKUs related to delivery
    filter(!(SKU %in% c(411554, 411509, 431934, 437417, 411547,
                        431903, 431910, 411578, 411592, 411523,
                        411516, 437806, 610551, 432016, 437813,
                        431941, 477949, 610568, 433464, 431958, 800112, 411530,
                        866088 # gift with purchase
                        ))) %>% 
    group_by(sales_month, SKU) %>% 
    summarise(
      transaction_number = paste(unique(transaction_number), collapse = ","),
      SKU_appearance = length(SKU)
    ) %>% 
    group_by(sales_month) %>% 
    mutate(rank = dense_rank(desc(SKU_appearance))) %>% 
    ungroup() %>% 
    arrange(sales_month, desc(SKU_appearance)) %>%
    filter(rank <= n)
  
  top_skus <- df_eCommerce_top_monthly %>% 
    filter(sales_month == as_date(date_string)) %>% 
    distinct(SKU, rank)
  
  retail_transactions_with_top_skus <- df_retail %>% 
    select(sales_month, transaction_number, SKU) %>% 
    filter(sales_month == as_date(date_string), SKU %in% top_skus$SKU) %>% 
    distinct(transaction_number) %>% pull()
  
  retail_basket <- df_retail %>% 
    select(sales_month, transaction_number, SKU) %>% 
    filter(sales_month == as_date(date_string), transaction_number %in% retail_transactions_with_top_skus) %>% 
    left_join(top_skus, by = "SKU") %>% 
    mutate(SKU = ifelse(!is.na(rank), paste0("(Top ", rank, ")", SKU), SKU)) %>% 
    arrange(transaction_number, desc(rank))
  
  trans <- retail_basket %>% 
    group_by(transaction_number) %>%
    summarise(basket = paste(SKU, collapse = ",")) %>% 
    pull(basket)
  trans <- as(strsplit(trans,',',fixed=T), "transactions")
  
  top_skus <- left_join(
    top_skus,
    df_eCommerce %>% select(SKU, Product_Description) %>% 
      filter(SKU %in% top_skus$SKU) %>% distinct(),
    by = "SKU"
  ) %>% 
    mutate(sales_month = as_date(date_string)) %>% 
    select(sales_month, `eCommerce SKU` = SKU, rank, `Product Description` = Product_Description)
  
  list(
    top_skus = top_skus, 
    trans = trans
  )
}

all_dates <- seq.Date(as_date("2017-12-01"), as_date("2019-11-01"), by = "month")
top_skus_list <- vector("list", length(all_dates))
names(top_skus_list) <- all_dates
trans_list <- vector("list", length(all_dates))
names(trans_list) <- all_dates

for (i in seq_along(all_dates)) {
  dateString <- all_dates[i]
  res <- make_retail_basket(10, dateString)
  top_skus_list[[i]] <- res$top_skus
  trans_list[[i]] <- res$trans
  gc(verbose = F)
}

bind_rows(top_skus_list) %>% DT::datatable(rownames = F)

saveGIF({
    for (i in seq_along(all_dates)) {
      dateString <- all_dates[i]
      itemFrequencyPlot(trans_list[[i]], topN=20,
                        type="absolute",
                        col=brewer.pal(8,'Pastel2'),
                        main = paste0("Top 20 Retail Absolute Item Frequency Plot (", dateString, ")"))
    }
  },
  movie.name = "Top20RetailAbsoluteItemFreq.gif", 
  interval = .8, 
  ani.width = 600, 
  ani.height = 500,
  outdir = getwd()
)

saveGIF({
  for (i in seq_along(all_dates)) {
    dateString <- all_dates[i]
    itemFrequencyPlot(trans_list[[i]], topN=20,
                      type="relative",
                      col=brewer.pal(8,'Pastel2'),
                      main = paste0("Top 20 Retail Relative Item Frequency Plot (", dateString, ")"))
  }
}, movie.name = "Top20RetailRelativeItemFreq.gif", 
interval = .8, 
ani.width = 600, 
ani.height = 500,
outdir = getwd()
)

# Modeling ====
# i <- 1
# top_skus <- top_skus_list[[i]]
# trans <- trans_list[[i]]

do_MBA <- function(trans, top_skus, supp=0.001, conf = 0.5, minlen = 2, maxlen = 10) {
  lhs_top_skus <- top_skus %>%
    transmute(`eCommerce SKU` = paste0("(Top ", rank, ")", `eCommerce SKU`)) %>% pull()
  
  rules_list <- vector("list", length(lhs_top_skus))
  names(rules_list) <- lhs_top_skus
  
  for (j in seq_along(rules_list)) {
    # those who bought "top N" also bought
    try({
      association.rules <- apriori(
        trans,
        parameter = list(supp=supp, conf = conf, minlen = minlen, maxlen = maxlen),
        appearance = list(default = "rhs", lhs = names(rules_list)[j]),
        control = list(verbose = F)
      )
      if(length(association.rules) > 0) {
        print(paste0("===========", names(rules_list)[j]))
        res <- as_tibble(inspect(association.rules), validate = F)
        colnames(res) <- c("From", "direction", "To", "support", "confidence", "lift", "count")
        rules_list[[j]] <- res
      }
    })
  }
  bind_rows(rules_list)
}

do_MBA_for_all <- function(supp=0.001, conf = 0.5, minlen = 2, maxlen = 10) {
  ruleset_list <- vector("list", length(top_skus_list))
  names(ruleset_list) <- names(top_skus_list)
  for (i in seq_along(ruleset_list)) {
    ruleset_list[[i]] <- do_MBA(trans_list[[i]], top_skus_list[[i]],
                                supp = supp, conf = conf, minlen = minlen)
  }
  bind_rows(ruleset_list,  .id = "sales_month")
}

model_conf_25 <- do_MBA_for_all(supp=0.001, conf = 0.25, minlen = 2, maxlen = 10)
model_conf_30 <- do_MBA_for_all(supp=0.001, conf = 0.30, minlen = 2, maxlen = 10)
model_conf_35 <- do_MBA_for_all(supp=0.001, conf = 0.35, minlen = 2, maxlen = 10)
model_conf_40 <- do_MBA_for_all(supp=0.001, conf = 0.40, minlen = 2, maxlen = 10)
model_conf_45 <- do_MBA_for_all(supp=0.001, conf = 0.45, minlen = 2, maxlen = 10)
model_conf_50 <- do_MBA_for_all(supp=0.001, conf = 0.50, minlen = 2, maxlen = 10)
model_conf_55 <- do_MBA_for_all(supp=0.001, conf = 0.55, minlen = 2, maxlen = 10)
model_conf_60 <- do_MBA_for_all(supp=0.001, conf = 0.60, minlen = 2, maxlen = 10)

library("xlsx")
write_to_excel <- function(model_res_df, sheetName) {
  # Get ecommerce sku product info
  model_res_df <- left_join(
    model_res_df %>% 
      filter(str_detect(str_to_lower(From), "top")) %>% 
      select(From) %>% 
      mutate(
        SKU = gsub("[{(Top }]", "", From),
        SKU = gsub("[0-9]+)", "", SKU)
      ) %>% transmute(From = From, SKU = as.numeric(SKU)),
    df_eCommerce %>% 
      select(SKU, `From Product Desc` = Product_Description, `From Category Desc` = Product_Category_Desc)
  ) %>% distinct() %>% 
    inner_join(
      model_res_df, by = "From"
    )
  
  # Get retail sku product info
  model_res_df <- left_join(
    model_res_df %>% 
      select(To) %>% 
      transmute(To = To, SKU = as.numeric(str_replace_all(To, "[[:punct:]]", ""))),
    df_retail %>% 
      select(SKU, `To Product Desc` = Product_Description, `To Category Desc` = Product_Category_Desc)
  ) %>% distinct() %>% 
    inner_join(
      model_res_df, by = "To"
    )
  model_res_df <- model_res_df %>% 
    select(sales_month, From, `From Product Desc`, `From Category Desc`,
           To, `To Product Desc`, `To Category Desc`, support, confidence, lift, count)
  model_res_df[is.na(model_res_df)] <- ""
  write.xlsx(as.data.frame(model_res_df),
             "model_SKU.xlsx",
             sheetName = sheetName, 
             col.names = TRUE, row.names = FALSE, append = TRUE)
}

write_to_excel(as.data.frame(model_conf_25), "model_conf_25")
write_to_excel(as.data.frame(model_conf_30), "model_conf_30")
write_to_excel(as.data.frame(model_conf_35), "model_conf_35")
write_to_excel(as.data.frame(model_conf_40), "model_conf_40")
write_to_excel(as.data.frame(model_conf_45), "model_conf_45")
write_to_excel(as.data.frame(model_conf_50), "model_conf_50")
write_to_excel(as.data.frame(model_conf_55), "model_conf_55")
write_to_excel(as.data.frame(model_conf_60), "model_conf_60")



# lhs_top_skus <- top_skus %>%
#   transmute(`eCommerce SKU` = paste0("(Top ", rank, ")", `eCommerce SKU`)) %>% pull()
# 
# rules_list <- vector("list", length(lhs_top_skus))
# names(rules_list) <- lhs_top_skus
# 
# for (j in seq_along(rules_list)) {
#   # those who bought "top N" also bought
#   try({
#     association.rules <- apriori(
#       trans,
#       parameter = list(supp=supp, conf = conf, minlen = minlen, maxlen = maxlen),
#       appearance = list(default = "rhs", lhs = names(rules_list)[j]),
#       control = list(verbose = F)
#     )
#     if(length(association.rules) > 0) {
#       print(paste0("===========", names(rules_list)[j]))
#       res <- as_tibble(inspect(association.rules), validate = F)
#       colnames(res) <- c("From", "direction", "To", "support", "confidence", "lift", "count")
#       rules_list[[j]] <- res
#     }
#   })
# }
# 
# # Support: Fraction of transactions that contain the A = > B
# # Confidence: For a rule A=>B Confidence shows the percentage in which B is bought with A.
# # Lift: Lift gives the correlation between A and B in the rule A=>B. 
# #       Correlation shows how one item-set A effects the item-set B.
# # Lift is the ratio of the support of the A => B, divided by 
# #   the probability that the A and B co-occur if the two are independent.
# # Filter rules with confidence greater than 0.4 or 40%
# A lift greater than 1 suggests that the presence of the antecedent increases 
#   the chances that the consequent will occur in a given transaction
# Lift below 1 indicates that purchasing the antecedent reduces the chances of purchasing the consequent 
#   in the same transaction. Note: This could indicate that the items are seen by customers as alternatives to each other
# 
# When the lift is 1, then purchasing the antecedent makes no difference on the chances of purchasing the consequent
#
#
#
# association.rules <- apriori(trans, parameter = list(supp=0.001, conf=0.8, maxlen=10))
# summary(association.rules)
# inspect(association.rules[1:10])
# subRules <- association.rules[quality(association.rules)$confidence>0.4]
# # Plot SubRules
# plot(subRules)
# plot(subRules,method="two-key plot")
# 
# top10subRules <- head(subRules, n = 10, by = "confidence")
# plot(top10subRules, method = "graph",  engine = "htmlwidget")
# 
# subRules2 <- head(subRules, n=20, by="lift")
# plot(subRules2, method="paracoord")






