# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
# If you have already seeded the db and made changes to this file,
# run `rails db:reset`. Running `rails db:seed` will fail because rails
# will try to create another user with the same email
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)
if Rails.env == 'development'
  u = User.create!(email: 'example@example.com', password: '123456')
  u.make_the_hottest_of_takes! "Spiderwick Chronicles >> Bridge to Terabithia"
  u.make_the_hottest_of_takes! "Tea is better lukewarm"
end
