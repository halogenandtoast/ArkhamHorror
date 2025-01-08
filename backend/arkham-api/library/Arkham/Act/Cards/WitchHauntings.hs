module Arkham.Act.Cards.WitchHauntings (WitchHauntings (..), witchHauntings) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype WitchHauntings = WitchHauntings ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntings :: ActCard WitchHauntings
witchHauntings = act (2, A) WitchHauntings Cards.witchHauntings Nothing

instance HasAbilities WitchHauntings where
  getAbilities (WitchHauntings a) =
    [ mkAbility a 1 $ Objective $ FastAbility $ ClueCost (PerPlayer 1)
    | onSide A a
    ]

instance HasModifiersFor WitchHauntings where
  getModifiersFor (WitchHauntings a) = do
    locations <- modifySelectMaybe a (LocationIsInFrontOf Anyone) \lid -> do
      iid <- MaybeT $ field LocationInFrontOf lid
      pure
        [ ConnectedToWhen (LocationWithId lid)
            $ not_ (LocationWithId lid)
            <> LocationIsInFrontOf (InvestigatorWithId iid)
        ]

    investigators <- modifySelectMaybe a Anyone \iid -> do
      lids <- lift $ select $ LocationIsInFrontOf (not_ $ InvestigatorWithId iid)
      pure $ map CannotEnter lids

    pure $ locations <> investigators

instance RunMessage WitchHauntings where
  runMessage msg a@(WitchHauntings attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithClues
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ AdvanceActDeck (actDeckId attrs) (toSource attrs)
      pure a
    _ -> WitchHauntings <$> runMessage msg attrs
