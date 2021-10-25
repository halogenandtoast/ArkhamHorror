module Arkham.Types.Investigator.Cards.DaisyWalker
  ( DaisyWalker(..)
  , daisyWalker
  ) where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target

newtype DaisyWalker = DaisyWalker InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

-- TODO: Tome action should be a modifier and actions should quantify restrictions
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

instance HasTokenValue env DaisyWalker where
  getTokenValue (DaisyWalker attrs) iid ElderSign | iid == toId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 0)
  getTokenValue _ _ token = pure $ TokenValue token mempty

-- Passing a skill test effect
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
    PassedSkillTest iid _ _ (TokenTarget token) _ _ | iid == investigatorId ->
      case tokenFace token of
        ElderSign -> do
          tomeCount <- unAssetCount <$> getCount (iid, [Tome])
          i <$ when (tomeCount > 0) (push $ DrawCards iid tomeCount False)
        _ -> pure i
    BeginRound -> DaisyWalker
      <$> runMessage msg (attrs { investigatorTomeActions = Just 1 })
    _ -> DaisyWalker <$> runMessage msg attrs
