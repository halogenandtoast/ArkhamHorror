module Arkham.Investigator.Cards.DaisyWalker
  ( DaisyWalker(..)
  , daisyWalker
  ) where

import Arkham.Prelude

import Arkham.Asset.Attrs ( Field (..) )
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Source
import Arkham.Target

newtype DaisyWalker = DaisyWalker InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisyWalker :: InvestigatorCard DaisyWalker
daisyWalker = investigatorWith
  DaisyWalker
  Cards.daisyWalker
  Stats
    { health = 5
    , sanity = 9
    , willpower = 3
    , intellect = 5
    , combat = 2
    , agility = 2
    }
  (tomeActionsL ?~ 1)

instance HasTokenValue DaisyWalker where
  getTokenValue iid ElderSign (DaisyWalker attrs) | iid == toId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 0)
  getTokenValue _ token _ = pure $ TokenValue token mempty

-- Passing a skill test effect
instance RunMessage DaisyWalker where
  runMessage msg i@(DaisyWalker attrs@InvestigatorAttrs {..}) = case msg of
    ResetGame -> do
      attrs' <- runMessage msg attrs
      pure $ DaisyWalker $ attrs' & tomeActionsL ?~ 1
    SpendActions iid (AssetSource aid) actionCost
      | iid == toId attrs && actionCost > 0 -> do
        isTome <- fieldP AssetTraits (member Tome) aid
        DaisyWalker
          <$> if isTome
                  && fromJustNote "Must be set" investigatorTomeActions
                  > 0
                then runMessage
                  (SpendActions iid (AssetSource aid) (actionCost - 1))
                  (attrs & tomeActionsL %~ fmap (max 0 . subtract 1))
                else runMessage msg attrs
    PassedSkillTest iid _ _ (TokenTarget token) _ _ | iid == investigatorId ->
      do
        when (tokenFace token == ElderSign) $ do
          tomeCount <-
            selectCount
            $ assetControlledBy investigatorId
            <> AssetWithTrait Tome
          when (tomeCount > 0) (push $ DrawCards iid tomeCount False)
        pure i
    BeginRound -> DaisyWalker <$> runMessage msg (attrs & tomeActionsL ?~ 1)
    _ -> DaisyWalker <$> runMessage msg attrs
