% conversations. type=p2p, group, game
-record(user, {id, username, sex, email, password, facebook_id}).
-record(conv, {id, type, stamp}).
-record(conv_msg, {conv_id, last_id}).
-record(user_msg, {user_id, last_id}).
-record(message, {id, text, user_id, conv_id}).
-record(user_conv, {id, user_id, conv_id, stamp}).
-record(user_online, {id, stamp, pid, user_id}).
