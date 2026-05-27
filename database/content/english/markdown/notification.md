#### Data Upload Instructions

Please follow the format and field specifications below when uploading review data according to the sample files.

------------------------------------------------------------------------

##### 📋 Upload Method Selection

This system provides two upload methods. Please choose the most suitable one for you:

**🔸 Single File Upload**
- Suitable for: All data in the same file
- Operation: Click "Browse Files" button to select file

**🔸 Multiple File Upload** ⭐ **Recommended for Multi-Brand Analysis**
- Suitable for: Different brands or products in separate files
- Operation: Click "Select Multiple Files", hold `Ctrl` (Windows) or `Cmd` (Mac) to select multiple files at once
- Advantage: System automatically merges all files and removes duplicate data

------------------------------------------------------------------------

##### 1. File Format

-   **File Type**: Excel files (.xlsx, .xls) or CSV files (.csv)
-   **Worksheet**: For Excel files, please place data in the first worksheet (Sheet1)
-   **Encoding**: Please use UTF-8 encoding for CSV files
-   **Header Row**: Ensure the first row contains field names with no empty rows

------------------------------------------------------------------------

##### 2. Review Data Field Description

| Field Name | Data Type | Required | Description |
|----|----|----|----|
| `Variation` | String | Yes | Product/brand identifier (e.g., brand name) |
| `ASIN` | String | No | Amazon Standard Identification Number (can replace Variation) |
| `Title` | String | Yes | Review title, brief and concise; recommended length 10～100 characters |
| `Body` | String | Yes | Review content, complete usage experience; recommended length 50～1000 characters |

📝 **Field names are case-insensitive**: System automatically recognizes `variation`, `Variation`, `VARIATION`, etc.

⚠️ **English column names recommended**: For best compatibility, use English column names (e.g., `Variation`, `Title`, `Body`)

------------------------------------------------------------------------

##### 3. Sales Data Field Description (Optional)

For sales regression analysis, please upload sales data separately:

| Field Name | Data Type | Required | Description |
|----|----|----|----|
| `Variation` | String | Yes | Product/brand identifier (must match review data) |
| `ASIN` | String | No | Amazon Standard Identification Number (can replace Variation) |
| `Sales` | Numeric | Yes | Sales quantity |
| `payment_time` | Date | No | Transaction date (format: YYYY-MM-DD) |
| `price` | Numeric | No | Unit price |

📝 **Sales data Variation or ASIN must match review data** for proper analysis

------------------------------------------------------------------------

##### 4. Sample Data

| Variation | Title | Body |
|----|----|----|
| Brand A | Great value product | This product has excellent value for money, very user-friendly, highly recommended |
| Brand B | Quality needs improvement | Started having issues after one month of use, customer service is decent but product quality needs enhancement |
| Brand A | Very satisfied | Exceeded expectations! Full-featured, beautifully designed, will recommend to friends |

------------------------------------------------------------------------

##### 5. Multiple File Upload Example

If you have data for multiple brands in separate files:

📁 **File 1: Brand_A_Reviews.xlsx**
| Variation | Title | Body |
|----|----|----|
| Brand A | Excellent product | Used for a long time, great quality... |

📁 **File 2: Brand_B_Reviews.csv**
| Variation | Title | Body |
|----|----|----|
| Brand B | Decent | Average product, nothing particularly impressive... |

👆 System will automatically merge into one complete data table for analysis

------------------------------------------------------------------------

##### 6. Important Notes

1.  **Required Field Verification**: Please ensure data contains required fields (Variation, Title, Body)
2.  **Null Value Check**: All required fields cannot be empty; please complete missing data before upload
3.  **Duplicate Check**: System automatically removes duplicate reviews (same Variation + Title + Body)
4.  **Character Encoding**: Please save CSV files in UTF-8 encoding to avoid Chinese character issues
5.  **File Size**: Recommend single file not exceeding 10MB, total not exceeding 50MB
6.  **File Naming**: Recommend meaningful file names, e.g., `Brand_A_Reviews_20240101.xlsx`

------------------------------------------------------------------------

##### 💡 Operation Tips

- **Multiple File Selection**: Hold `Ctrl` (Windows) or `Cmd` (Mac) key, then click files to select
- **File Verification**: Before upload, open files with Excel or text editor to confirm correct format
- **Troubleshooting**: If upload fails, please check file format and required fields are complete