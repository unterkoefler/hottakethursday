class Take < ApplicationRecord
  belongs_to :user
  has_many :votes

  validates_length_of :contents,
                      minimum: 10,
                      maximum: 140,
                      allow_blank: false

  def number_of_likes
    self.votes.count
  end

  def users_who_liked
    self.votes.map(&:user)
  end
end
