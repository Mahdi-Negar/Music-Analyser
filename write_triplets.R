####### read triplets

# create table triplets 
# (usersha1 character(40), 
#   songid character(18), 
#   plays integer);
# .separator "\t" "\n"
# .import train_triplets.txt triplets
# create index idx_usersha1 on triplets(usersha1);
# create index idx_plays on triplets(plays);
# create index idx_songid on triplets(songid);
