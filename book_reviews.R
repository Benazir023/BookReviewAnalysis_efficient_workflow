setwd("D:/BENA/Data Analytics/Dataquest/Project2_DataCleaning")
save.image("book_reviews.RData")
savehistory("book_reviews.Rhistory")

library(tidyverse)

#Import data

book_reviews <- read_csv("D:/BENA/Data Analytics/Dataquest/Project2_DataCleaning/book_reviews.csv")

#About data

dim(book_reviews)
colnames(book_reviews)
typeof(book_reviews)

#Data type for each column

for (col in colnames(book_reviews)) {
 print(typeof(book_reviews[[col]]))
}

#Unique values in each column

for (val in colnames(book_reviews)) {
  print("unique values")
  print(val)
  print(unique(book_reviews[[val]]))
}

#Handling missing data

book_reviews_nonulls = book_reviews %>%
  filter(!is.na(review))

dim(book_reviews_nonulls)
View(book_reviews_nonulls)

#Investigate missing data

book_reviews_nulls = book_reviews %>%
  filter(is.na(review))

View(book_reviews_nulls)

nulls_per_book <- book_reviews_nulls %>%
  group_by(book) %>%
  summarise(
    nulls = n()
  ) %>%
  arrange(-nulls)

nulls_per_state <- book_reviews_nulls %>%
  group_by(state)%>%
  summarise(
    nulls = n()
  ) %>%
  arrange(-nulls)

###Some 206 rows with nulls in the reviews column were deleted. We still have 89.7% of our data available
#Standardize values in state column

book_reviews_nonulls <- book_reviews_nonulls %>%
  mutate(
    state = case_when(
      state == "Texas" ~ "TX",
      state == "California" ~ "CA",
      state == "Florida" ~ "FL",
      state == "New York" ~ "NY",
      TRUE ~ state
    )
  )

View(book_reviews_nonulls)

#Transform text data to number type data

book_reviews_nonulls <- book_reviews_nonulls %>%
  mutate(
    review_num = case_when(
      review == "Poor" ~ 1,
      review == "Fair" ~ 2,
      review == "Good" ~ 3,
      review == "Great" ~ 4,
      review == "Excellent" ~ 5
    )
  )

book_reviews_nonulls <- book_reviews_nonulls %>%
  mutate(
    is_high_review = if_else(review_num >= 4,TRUE,FALSE) 
  )

#Finding the most profitable book

profitable_book <- book_reviews_nonulls %>%
  group_by(book) %>%
  summarize(
    total_price = sum(price)
  ) %>%
  arrange(-total_price)

###Secrets of R For Advanced Students had the highest total, alternatively considering the number of books sold

profitable_book <- book_reviews_nonulls %>%
  group_by(book) %>%
  summarize(
    no_purchased = n()
  ) %>%
  arrange(-no_purchased)

###Fundamentals of R For Beginners had the highest number of purchases








