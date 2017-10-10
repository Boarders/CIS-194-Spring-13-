data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinListm a) (JoinList m a)
  deriving (Eq, Show)
