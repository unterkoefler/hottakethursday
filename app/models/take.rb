class Take < ApplicationRecord
  belongs_to :user
  has_many :likes

  validates_length_of :contents,
                      minimum: 10,
                      maximum: 140,
                      allow_blank: false

  def number_of_likes
    likes.count
  end

  def users_who_liked
    likes.map(&:user)
  end
end
