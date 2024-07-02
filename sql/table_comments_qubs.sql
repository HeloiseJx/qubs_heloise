SELECT 
id as comment_id, 
user_id, 
model as comment_type, 
resource_id, 
resource_type,
created_at as date_comment
FROM qubs.comments