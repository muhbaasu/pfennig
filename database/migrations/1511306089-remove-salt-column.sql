begin;

alter table users drop column if exists salt;

comment on column users.password is 'The salted password of this user';

commit;
