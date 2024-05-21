# [Analyzing Sales Performance: E-Commerce Insights from Indian Market]

## Introduction

Welcome to the world of e-commerce sales analysis! In this dataset, i delved into the dynamic landscape of Indian e-commerce, exploring the intricacies of sales performance through meticulously curated data. the dataset, sourced from Kaggle, comprises three comprehensive CSV files: List of Orders, Order Details, and Sales Targets.

## Dataset Overview

1.	List of Orders: This dataset offers a detailed glimpse into purchase information, featuring essential data points such as Order ID, Date of Purchase, and Customer Details.

2.	Order Details: Dive deeper into the specifics of this dataset, which includes Order ID, Order Price, Quantity, Profit, and detailed Category and Subcategory information.

3.	Sales Target: Gain insight into the overarching goals and aspirations of each product category with this dataset, showcasing sales target amounts and dates.

## Loading Package
I started by loading and installing the package required for necessary data cleaning, analysis and visualization

```{r}
library(tidyverse)
```

```{r}
library(lubridate)
```

## Load Datasets
The next step was to load the dataset required for analysis
```{r}
Order_List<- read.csv("C:\\Users\\USER\\Documents\\PERSONAL\\DATA ANALYTICS\\My Portfolio\\R Studio\\List of Orders.csv")
Order_Details<- read.csv("C:\\Users\\USER\\Documents\\PERSONAL\\DATA ANALYTICS\\My Portfolio\\R Studio\\Order Details.csv")
Sales_Target<- read.csv("C:\\Users\\USER\\Documents\\PERSONAL\\DATA ANALYTICS\\My Portfolio\\R Studio\\Sales Target.csv")
```

## Data Cleaning and Exploration
I cleaned and explored each dataset for analysis

### Order_List Dataset

```{r}
glimpse(Order_List)
```
I converted the 'order_Date' column from character datatype to date datatype

```{r}
Order_List$Order.Date<- dmy(Order_List$Order.Date)
glimpse(Order_List)
```
### Order_Details Dataset

```{r}
glimpse(Order_Details)
```
### Sales_Target Dataset

```{r}
glimpse(Sales_Target)
```
## Summary Statistics

Let's summarize the dataset before carrying further analysis and answering the business questions

```{r}
summary(Order_List)
```
```{r}
summary(Order_Details)
```

```{r}
summary(Sales_Target)
```
## Business Questions

1. What is the total sales revenue for each month?

In answering this question, i joined both the Order_List and Order_Details table using the inner join function

```{r}
Sales_Table<- inner_join(Order_List, Order_Details, by = "Order.ID")
```

Afterwards, i calculated the sales of each transaction

```{r}
Sales_Table<- mutate(Sales_Table, Sales = Amount * Quantity)
```

Additionally, I formatted the date column to a character datatype to display both the month names and year when calculating the sales for each month.

```{r}
Sales_Table$Order.Date<- format(Sales_Table$Order.Date, format = "%b-%y")
glimpse(Sales_Table)
```
Finally,i calculated the sales for each month
```{r}
Sales_Table %>% 
  select(Order.Date, Sales) %>% 
  group_by(Order.Date) %>% 
  summarise(Total_Sales = sum(Sales)) %>% 
  arrange(desc (Total_Sales))
```
 Visualizing the above

```{r}
ggplot(Sales_Table, aes(x = Order.Date, y = Sales)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Monthly Performance Analysis", X = "Date", y = "Total Sales", caption = "Data sourced from Kaggle")+
  theme(axis.text.x = element_text(angle = 90)) +  theme(plot.title = element_text(hjust = 0.4))
```
 
 * The top three performing months were January-2019, March-2019 and November-2018

2. Which category has the highest sales revenue in the given period?
```{r}
Sales_Table %>% 
  select(Category, Sales) %>% 
  group_by(Category) %>% 
  summarise(Total_Sales = sum(Sales)) %>% 
  filter(Total_Sales == max(Total_Sales))
```
 * From the above, Electronics was the top performing product category
  
3.	How does the sales target for each category vary month by month?

```{r}
Sales_Target %>%
    select(Category, Target, Month) %>%
    group_by(Month, Category) %>% 
    summarise(Total_Target = sum(Target))
```
 
 * The monthly targets for each product category vary. For Furniture, the monthly target changes range from +100 to +500 units. Clothing targets were increased by 2000 units quarterly in 2018, but remained constant after an increase in Q3 2018. Electronics targets were steady throughout 2018 but saw a significant 77% increase from 9000 to 16,000 units per month in January 2019, remaining constant thereafter.

4.	Which city has the highest number of orders?

```{r}
Sales_Table %>% 
  select(City, Quantity) %>% 
  group_by(City) %>% 
  summarise(Order_Count = sum(Quantity)) %>% 
  filter(Order_Count == max(Order_Count))
```
 * The city with the highest number of orders was Indore

5.	What is the average profit margin for each category?

Firstly, i calculated the profit margin of each transaction 

```{r}
Sales_Table<- mutate(Sales_Table, Profit_Margin = (Profit/Sales)* 100)
```
I then calculated the average profit margin for each category and housed it in a new variable

```{r}
P_Margin <- Sales_Table %>% 
  select (Category, Profit_Margin) %>% 
  group_by(Category) %>% 
  summarise(Avg_Profit_Margin = mean(Profit_Margin))
print(P_Margin)
```

finally, i rounded off the output and Concatenated the "%" symbol

```{r}
P_Margin$Avg_Profit_Margin<-round(P_Margin$Avg_Profit_Margin,2)
P_Margin$Avg_Profit_Margin<- paste0(P_Margin$Avg_Profit_Margin, "%")
print(P_Margin)
```
 * From the above analysis, Clothing has the highest Avg.Profit Margin followed by Electronics, while Furniture is currently being sold at a loss.

6.	Is there any correlation between the quantity of products ordered and the profit generated?

In answering the above, i created object variables for both quantity and product before calculating the correlation

```{r}
Profit_obj<- Sales_Table$Profit
Quantity_obj<- Sales_Table$Quantity
```
 
 I then calculated the correlation

```{r}
correlation<- cor(Quantity_obj, Profit_obj)
print(correlation)
```
Furthermore, i also analysed the correlation using a scattered plot

```{r}
ggplot(data = Sales_Table, aes(x = Quantity, y = Profit))+
  geom_point() + labs(title = "A Correlation plot between Quantity and Profit", x = "Quantity", y = "Profit", 
                      caption = "Data sourced from Kaggle")
```
 * Examining the output and plot, it's clear that there is a very low correlation between Quantity and Profit. This indicates that profit does not rely on the quantity sold. In other words, selling more items does not necessarily lead to higher profits. This suggests that other factors, such as pricing, cost of goods, or operational efficiency, might be influencing profitability more than the sheer volume of sales.

7.	What is the percentage achievement of sales targets for each category?

In resolving the above business question, i started by joining both Sales_Table and Sales_Target tables using the Full Join function and housed it in a new variable

```{r}
Target_Table<- full_join(Sales_Table, Sales_Target)
```

I went ahead to calculate the percentage achieved for each category

```{r}
Target_Achieved<-Target_Table %>% 
  select(Category, Sales, Target) %>% 
  group_by(Category) %>% 
  summarise(Total_Sales = sum(Sales),
            Total_Sales_Target = sum(Target),
            Percentage_Achieved = (Total_Sales/Total_Sales_Target)*100)
print(Target_Achieved)
```

Finally, i rounded off the output and Concatenated the "%" symbol

```{r}
Target_Achieved$Percentage_Achieved<-round(Target_Achieved$Percentage_Achieved, 2)
Target_Achieved$Percentage_Achieved <- paste0(Target_Achieved$Percentage_Achieved,"%")
print(Target_Achieved)
```
 *Based on the analysis, Furniture led in target achievement with a 24.74% achievement rate, closely matching the performance of Electronics. Despite running at a loss, as noted earlier, the data shows there is demand, and with adjustments, profitability could be Electronics ranked second with a 24.66% achievement rate which is better than Clothing but still shows room for improvement. The relatively higher achievement indicates stronger performance or possibly more effective sales strategies for this category. Clothing achieved only 4.83% of its sales target. This indicates a significant shortfall, suggesting potential issues in sales strategies or market conditions affecting clothing sales. However, it is important to note that higher targets are often allocated to clothing, possibly due to its higher profitability compared to other categories.

8.	How does the sales performance vary across different states?

```{r}
State_Sales<- Sales_Table %>% 
  select(State, Sales) %>% 
  group_by(State) %>% 
  summarise(Tol_Sales = sum(Sales)) %>% 
  arrange(desc (Tol_Sales))
 print(State_Sales)
```

Visualizing the above

```{r}
State_Sales$State<- factor(State_Sales$State, levels = State_Sales$State[order(-State_Sales$Tol_Sales)])
```

```{r}
ggplot(State_Sales, aes(x = State, y = Tol_Sales)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "State Performance Analysis", X = "State", y = "Sales", caption = "Data sourced from Kaggle")+
  theme(axis.text.x = element_text(angle = 90)) +  theme(plot.title = element_text(hjust = 0.4))
```
 * Madhya Pradesh was the top performing state for the period under review

9.	Can we identify any monthly seasonal trends in sales for each category?
```{r}
ggplot(Sales_Table, aes(x= Order.Date, y= Sales, colour = Category)) +
  geom_line() +
  labs(title = "Monthly Seasonal Trend in Sales", x = "Year", y ="Sales", caption = "Data sourced from Kaggle")+
  scale_color_manual(values = c("Clothing" = "red", "Electronics" = "magenta", "Furniture" = "blue")) +
  facet_wrap(~Category)+ theme(axis.text.x = element_text(angle = 90))
```
 * from the above analysis, Clothing is more sold in August which is summer season. For Electronics, more sales was recorded in the month of January (during winter seasons) while for Furniture, more sales was recorded in March (Spring).

10.	Are there any outliers in terms of order amount or profit that need further investigation?

```{r}
ggplot(data = Sales_Table, aes(x = Amount, y = Profit))+
  geom_point(fill = "blue") + labs(title = "A Scattered plot between Amount and Profit", x = "Amount", y = "Profit", 
                                   caption = "Data sourced from Kaggle") 
```
 * The scattered plot between Amount and Profit reveals several data points that can be considered outliers, which may require further investigation:

1. High Amount, Low Profit: There are points with high amounts (greater than 4000) but relatively low profit. These outliers indicate transactions where a large amount was involved but did not yield proportional profit, suggesting possible inefficiencies, pricing issues, or unexpected costs.

2. High Loss: A few data points show significant losses (ranging from -1000 to -2000) at various amount levels. These outliers may indicate problematic transactions where the cost exceeded the revenue by a substantial margin, warranting investigation into the reasons behind these high losses.

3. Isolated Points: Points that are isolated from the main cluster, especially the one with a profit higher than 1000 or a loss lower than -1000, are outliers. These could be due to exceptional circumstances, errors, or unique cases that differ significantly from the typical transaction.

To Conclude, the outliers in the scatter plot, which are far from the main cluster of points, suggest areas where business processes or transactions need to be examined. These points indicate unusual transactions that might reveal issues or anomalies that should be investigated further.

## Conclusion

The analysis of the Indian e-commerce dataset reveals insightful trends in sales performance across different product categories. Electronics emerged as the top-performing category with the highest total sales revenue. Furniture, while also having significant sales, shows a negative average profit margin, indicating it is being sold at a loss. Clothing, despite having the highest average profit margin at 1.68%, had the lowest percentage achievement of sales targets at 4.83%. Additionally, Indore recorded the highest number of orders among all cities. This comprehensive analysis underscores the critical need for category-specific strategies to enhance profitability and achieve sales targets effectively.



