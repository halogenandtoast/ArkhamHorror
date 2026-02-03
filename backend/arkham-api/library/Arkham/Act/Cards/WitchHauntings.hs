module Arkham.Act.Cards.WitchHauntings (witchHauntings) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection

newtype WitchHauntings = WitchHauntings ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntings :: ActCard WitchHauntings
witchHauntings = act (2, A) WitchHauntings Cards.witchHauntings Nothing

instance HasAbilities WitchHauntings where
  getAbilities (WitchHauntings a) =
    [ restricted a 1 (DuringTurn Anyone) $ Objective $ FastAbility $ ClueCost (PerPlayer 1)
    | onSide A a
    ]

instance HasModifiersFor WitchHauntings where
  getModifiersFor (WitchHauntings a) = do
    modifySelectMaybe a (LocationIsInFrontOf Anyone) \lid -> do
      iid <- MaybeT $ field LocationInFrontOf lid
      pure
        [ ConnectedToWhen (LocationWithId lid)
            $ not_ (LocationWithId lid)
            <> LocationIsInFrontOf (InvestigatorWithId iid)
        ]

    modifySelectMaybe a Anyone \iid -> do
      lids <- lift $ select $ LocationIsInFrontOf (not_ $ InvestigatorWithId iid)
      pure $ map CannotEnter lids

instance RunMessage WitchHauntings where
  runMessage msg a@(WitchHauntings attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> WitchHauntings <$> liftRunMessage msg attrs
