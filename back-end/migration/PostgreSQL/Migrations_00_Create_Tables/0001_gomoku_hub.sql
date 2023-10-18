BEGIN transaction;

create extension IF NOT EXISTS  citext;
create extension IF NOT EXISTS pgcrypto;

CREATE SCHEMA IF NOT EXISTS  gomoku_hub;


create table IF NOT EXISTS gomoku_hub.users (
  id bigserial primary key not null,
  username text not null,
  passwd text not null,
  created timestamp with time zone default (now() at time zone 'utc')
);

create table IF NOT EXISTS gomoku_hub.game_types (
  id int primary key not null,
  game_type_name text not null
);

create table IF NOT EXISTS gomoku_hub.rooms (
  id bigserial primary key not null,
  game_type_id int not null,
  creator_user_id bigint,
  anon_creator boolean,
  board_state text,
  game_result text,
  FOREIGN KEY (game_type_id) REFERENCES gomoku_hub.game_types(id),
  FOREIGN KEY (creator_user_id) REFERENCES gomoku_hub.users(id)
);

CREATE INDEX IF NOT EXISTS IX_GOMOKU_HUB_rooms ON gomoku_hub.rooms (creator_user_id);

create table IF NOT EXISTS gomoku_hub.room_participants (
  room_id bigint not null,
  user_id bigint not null,
  FOREIGN KEY (room_id) REFERENCES gomoku_hub.rooms(id),
  FOREIGN KEY (user_id) REFERENCES gomoku_hub.users(id)
);

CREATE INDEX IF NOT EXISTS IX_GOMOKU_HUB_room_participants ON gomoku_hub.room_participants (user_id);

create table IF NOT EXISTS gomoku_hub.room_chat (
  room_id bigint not null,
  user_id bigint,
  anon_user bigint,
  message text not null,
  sent timestamp with time zone,
  FOREIGN KEY (room_id) REFERENCES gomoku_hub.rooms(id),
  FOREIGN KEY (user_id) REFERENCES gomoku_hub.users(id)
);

CREATE INDEX IF NOT EXISTS IX_GOMOKU_HUB_room_chat ON gomoku_hub.room_chat (room_id);


END TRANSACTION;