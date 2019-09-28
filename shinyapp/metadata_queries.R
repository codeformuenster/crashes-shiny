category_table <- 
    dbGetQuery(db_con, 
               paste0("SELECT ",
                      "(data->>'key')::integer as key, ",
                      "(data->>'title')::text as title, ",
                      "(data->>'title_brief')::text as title_brief, ",
                      "(data->>'measures')::text as measures ",
                      "FROM objects WHERE parent_id = '/buckets/accidents/collections/accident_category'"))
  
  kind_table <- 
    dbGetQuery(db_con, 
               paste0("SELECT ",
                      "(data->>'key')::integer as kind, ",
                      "(data->>'title')::text as title, ",
                      "(data->>'description')::text as description ",
                      "FROM objects WHERE parent_id = '/buckets/accidents/collections/accident_kind'"))
  
  # print(kind_table)
  
  type_table <- 
    dbGetQuery(db_con, 
               paste0("SELECT ",
                      "(data->>'key')::integer as type, ",
                      "(data->>'color')::text as color, ",
                      "(data->>'code')::text as code, ",
                      "(data->>'title')::text as title, ",
                      "(data->>'description')::text as description ",
                      "FROM objects WHERE parent_id = '/buckets/accidents/collections/accident_type'"))
  
  print(type_table)
  
    cause_table <- 
    dbGetQuery(db_con, 
               paste0("SELECT ",
                      "(data->>'key')::integer as number, ",
                      "(data->>'category')::text as category, ",
                      "(data->>'subcategory')::text as subcategory, ",
                      "(data->>'description')::text as description ",
                      "FROM objects WHERE parent_id = '/buckets/accidents/collections/accident_cause'"))
    
    # print(cause_table)
  