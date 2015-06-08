% conversations. type=p2p, group, game
-record(user, {id, stamp, username, firstname, lastname, gender, email, password, facebook_id, avatar}).
-record(conv, {id, stamp, type}).
-record(conv_msg, {conv_id, last_id}).
-record(user_msg, {user_id, last_id}).
-record(message, {id, text, stamp, user_id, conv_id}).
-record(user_conv, {id, user_id, conv_id, stamp}).
-record(user_online, {id, stamp, pid, user_id}).
