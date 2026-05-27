# DNA Analysis Results Guide

## What is DNA Analysis?

DNA (Data-driven, Nuanced Analysis) is a comprehensive customer value analysis system that helps you gain deep insights into customer behavioral characteristics.

## Core Metrics Explanation

### RFM Metrics Group

#### R - Recency
- **Definition**: Days since last purchase
- **Meaning**: Smaller numbers indicate higher activity; a key predictor of repurchase
- **Segmentation Logic**:
  - **High Active** (Low R): Recently purchased active customers
  - **General Active** (Medium R): Customers maintaining steady relationships
  - **At-Risk** (High R): Long time since purchase; require win-back

#### F - Frequency
- **Definition**: Total number of purchases during observation period
- **Meaning**: Reflects customer loyalty and engagement
- **Segmentation Logic**:
  - **High Frequency**: Many purchases, high loyalty
  - **Medium Frequency**: Regular purchase habits
  - **Low Frequency**: Infrequent purchases, require cultivation

#### M - Monetary
- **Definition**: Average purchase amount per transaction
- **Meaning**: Measures customer spending capacity and value
- **Segmentation Logic** (Using 80/20 Rule):
  - **High Value**: Top 20% contributing ~80% of revenue
  - **Medium Value**: Stable spending group
  - **Low Value**: Lower spending, growth potential

### Advanced Metrics

#### IPT - Inter-Purchase Time
- **Definition**: Average days between purchases
- **Meaning**: Understanding purchase cycles to optimize marketing timing
- **Application**: Set remarketing trigger points based on IPT

#### CAI - Customer Activity Index
- **Calculation**: CAI = (F × M) / R
- **Meaning**: Comprehensive assessment of overall customer activity
- **Segmentation**:
  - **High Active**: High frequency, high amount, recent purchase
  - **Medium Active**: Stable with room for improvement
  - **Low Active**: Potential customers needing activation

#### PCV - Past Customer Value
- **Definition**: Historical cumulative spending total
- **Meaning**: Measures customer's historical contribution to business
- **Segmentation** (80/20 Rule): Identify core contributing customers

#### CRI - Customer Retention Index
- **Calculation**: Based on standard deviation and mean of purchase frequency
- **Meaning**: Measures stability of customer purchase behavior
- **Segmentation**:
  - **High Stability**: Predictable, regular purchase patterns
  - **Medium Stability**: Generally regular with fluctuations
  - **Low Stability**: Unstable purchase behavior

#### NES - New-Entry-Seasoned Status
- **Definition**: Customer lifecycle stage based on purchase history
- **Categories**:
  - **N (New)**: New customer, first purchase
  - **E0 (Entry)**: Entry customer, few purchases
  - **S1, S2, S3 (Seasoned)**: Mature customers at different engagement levels

## How to Interpret Results

### 1. Statistical Summary
Review mean, median, and quartiles for each metric to understand overall customer distribution patterns.

### 2. Customer Segmentation
Each metric groups customers into segments (High/Medium/Low), each with:
- **Count & Percentage**: Number of customers in this group
- **Statistical Features**: Group's average, median, etc.
- **Marketing Suggestions**: Strategic recommendations for this group

### 3. AI Insights
The system automatically generates AI analysis including:
- **Overall Trends**: Main characteristics of customer base
- **Key Findings**: Anomalies or opportunities requiring attention
- **Action Items**: Specific executable strategies

## Marketing Application Recommendations

### High-Value Customers (High M or PCV)
- Provide VIP exclusive services
- Priority for new or premium product recommendations
- Build long-term relationships, increase loyalty

### At-Risk Customers (High R)
- Send win-back coupons
- Investigate churn reasons
- Provide personalized recommendations

### High-Frequency Customers (High F)
- Membership upgrade programs
- Rewards accumulation mechanisms
- Invite to become brand ambassadors

### Potential Customers (Low M but High F, or Medium CAI)
- Cross-sell and upsell
- Bundle promotions
- Educational content marketing

## Data Quality Checklist

Before DNA analysis, ensure:
- ✅ Data Completeness: Includes customer ID, purchase date, amount
- ✅ Time Range: At least 3-6 months of transaction records
- ✅ Data Accuracy: No duplicates or outliers
- ✅ Customer Identification: Same customer correctly merged

## Advanced Analysis Directions

1. **Customer Lifetime Value (CLV) Prediction**: Combine DNA metrics to forecast future value
2. **Churn Warning Model**: Predict churn risk based on R and F trend changes
3. **Segment Profiling**: Combine with demographic or behavioral data
4. **Product Recommendation System**: Recommend products based on customer DNA characteristics

---

**Tip**: DNA analysis is dynamic. Update monthly to track customer trend changes.
