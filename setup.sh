
sudo -u postgres psql <<EOF
CREATE USER "logiku_LOWER" password 'logiku';
CREATE DATABASE "logiku_LOWER_test" OWNER "logiku";
EOF
