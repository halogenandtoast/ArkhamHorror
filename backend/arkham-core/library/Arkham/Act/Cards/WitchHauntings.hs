module Arkham.Act.Cards.WitchHauntings (
  WitchHauntings (..),
  witchHauntings,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype WitchHauntings = WitchHauntings ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

witchHauntings :: ActCard WitchHauntings
witchHauntings = act (2, A) WitchHauntings Cards.witchHauntings Nothing

instance HasAbilities WitchHauntings where
  getAbilities (WitchHauntings a) =
    [ mkAbility a 1 $ Objective $ FastAbility $ ClueCost (PerPlayer 1)
    | onSide A a
    ]

instance HasModifiersFor WitchHauntings where
  getModifiersFor (LocationTarget lid) (WitchHauntings a) = do
    mInFrontOf <- field LocationInFrontOf lid
    pure
      $ toModifiers
        a
        [ ConnectedToWhen (LocationWithId lid)
          $ NotLocation (LocationWithId lid)
          <> LocationIsInFrontOf (InvestigatorWithId iid)
        | iid <- maybeToList mInFrontOf
        ]
  getModifiersFor (InvestigatorTarget iid) (WitchHauntings a) = do
    lids <-
      selectList
        $ LocationIsInFrontOf (NotInvestigator $ InvestigatorWithId iid)
    pure $ toModifiers a $ map CannotEnter lids
  getModifiersFor _ _ = pure []

instance RunMessage WitchHauntings where
  runMessage msg a@(WitchHauntings attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithClues
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ AdvanceActDeck (actDeckId attrs) (toSource attrs)
      pure a
    _ -> WitchHauntings <$> runMessage msg attrs
