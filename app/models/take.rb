class Take < ApplicationRecord
  belongs_to :user

  validates_length_of :contents,
                      minimum: 10,
                      maximum: 140,
                      allow_blank: false
end
