% conversations. type=p2p, group, game
-record(user, {id, stamp, username, firstname, lastname, gender, email, password, facebook_id, birthdate, city}).
-record(conv, {id, stamp, type}).
-record(message, {id, text, stamp, user_id, conv_id}).
-record(user_conv, {id, user_id, conv_id, stamp}).
-record(user_online, {id, stamp, pid, user_id, session_id}).
-record(user_file, {id, user_id, stamp, type, mime}).
