% conversations. type=p2p, group, game
-record(user, {id, stamp, username, firstname, lastname, gender, email, password, facebook_id, birthdate, city}).
-record(conv, {id, stamp, type}).
-record(message, {id, text, stamp, user_id, conv_id}).
-record(user_attr, {id, user_id, value}).
-record(user_conv, {id, user_id, conv_id, stamp}).
-record(user_online, {id, stamp, pid, user_id, session_id, online}).
-record(user_file, {id, user_id, stamp, type, mime}).
-record(user_friend, {id, stamp, type, user_id, friend_id}).
-record(user_session_data, {id, session_id, name, value}).

% todo

-record(todo,      {id, stamp, name, prio, move_to}).
-record(todo_item, {id, todo_id, prev_id, stamp, text}).
-record(todo_tag,  {id, todo_id, tag, default}).
-record(user_todo, {id, user_id, todo_id}).