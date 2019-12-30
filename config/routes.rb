Rails.application.routes.draw do
  # For details on the DSL available within this file, see https://guides.rubyonrails.org/routing.html
  scope :api do
    scope :v1, defaults: { format: :json } do
      devise_for :users,
                 controllers: { sessions: 'sessions',
                                registrations: 'registrations' }
      scope :users do
        get 'me', to: 'me#me'
        get 'by_id', to: 'me#by_id'
        get 'by_ids', to: 'me#by_ids'
      end

      scope :takes do
        get 'all', to: 'take#all'
        get 'all_from_today', to: 'take#all_from_today'
        post 'create', to: 'take#create'
        post 'like', to: 'take#like'
        post 'unlike', to: 'take#unlike'
      end
    end
  end

  # https://medium.com/@goncalvesjoao/rails-special-route-for-single-page-applications-de9e6bf32199
  root 'root#spa'
  get '*route', to: 'root#spa', constraints: ->(request) do
    !request.xhr? && request.format.html?
  end
end
