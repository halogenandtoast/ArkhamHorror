module Arkham.Types.Investigator.Cards.DaisyWalker
  ( DaisyWalker(..)
  , daisyWalker
  ) where


import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype DaisyWalker = DaisyWalker InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env DaisyWalker where
  getModifiersFor source target (DaisyWalker attrs) =
    getModifiersFor source target attrs

daisyWalker :: DaisyWalker
daisyWalker =
  DaisyWalker $ (baseAttrs "01002" "Daisy Walker" Seeker stats [Miskatonic])
    { investigatorTomeActions = Just 1
    }
 where
  stats = Stats
    { health = 5
    , sanity = 9
    , willpower = 3
    , intellect = 5
    , combat = 2
    , agility = 2
    }

instance ActionRunner env => HasActions env DaisyWalker where
  getActions i window (DaisyWalker attrs) = getActions i window attrs

instance HasTokenValue env DaisyWalker where
  getTokenValue (DaisyWalker attrs) iid ElderSign | iid == toId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 0)
  getTokenValue (DaisyWalker attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env DaisyWalker where
  runMessage msg i@(DaisyWalker attrs@InvestigatorAttrs {..}) = case msg of
    ResetGame -> do
      attrs' <- runMessage msg attrs
      pure $ DaisyWalker $ attrs' { investigatorTomeActions = Just 1 }
    SpendActions iid (AssetSource aid) actionCost
      | iid == toId attrs && actionCost > 0 -> do
        isTome <- elem Tome <$> getSet aid
        if isTome && fromJustNote "Must be set" investigatorTomeActions > 0
          then DaisyWalker <$> runMessage
            (SpendActions iid (AssetSource aid) (actionCost - 1))
            (attrs
              { investigatorTomeActions =
                max 0 . subtract 1 <$> investigatorTomeActions
              }
            )
          else DaisyWalker <$> runMessage msg attrs
    PassedSkillTest iid _ _ (DrawnTokenTarget token) _ _
      | iid == investigatorId -> case drawnTokenFace token of
        ElderSign -> do
          tomeCount <- unAssetCount <$> getCount (iid, [Tome])
          i <$ when
            (tomeCount > 0)
            (unshiftMessage $ chooseOne
              iid
              [ DrawCards iid tomeCount False
              , Continue "Do not use Daisy's ability"
              ]
            )
        _ -> pure i
    BeginRound -> DaisyWalker
      <$> runMessage msg (attrs { investigatorTomeActions = Just 1 })
    _ -> DaisyWalker <$> runMessage msg attrs
