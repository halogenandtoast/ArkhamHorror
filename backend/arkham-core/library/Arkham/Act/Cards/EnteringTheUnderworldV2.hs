module Arkham.Act.Cards.EnteringTheUnderworldV2 (EnteringTheUnderworldV2 (..), enteringTheUnderworldV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet (EncounterSet (Nightgaunts, StrikingFear, TerrorOfTheVale))
import Arkham.Helpers.Query (getInvestigators, getLead, getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Source
import Arkham.Trait (Trait (Vale))

newtype EnteringTheUnderworldV2 = EnteringTheUnderworldV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enteringTheUnderworldV2 :: ActCard EnteringTheUnderworldV2
enteringTheUnderworldV2 = act (1, A) EnteringTheUnderworldV2 Cards.enteringTheUnderworldV2 Nothing

instance HasAbilities EnteringTheUnderworldV2 where
  getAbilities (EnteringTheUnderworldV2 x) =
    [mkAbility x 1 $ forced $ EnemyDefeated #after Anyone ByAny $ EnemyWithClues $ atLeast 1]

instance RunMessage EnteringTheUnderworldV2 where
  runMessage msg a@(EnteringTheUnderworldV2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ GainClues iid (attrs.ability 1) 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      richardUptonPickman <- getSetAsideCard Assets.richardUptonPickman
      lead <- getLead
      investigators <- getInvestigators
      chooseOne
        lead
        [ targetLabel investigator [TakeControlOfSetAsideAsset investigator richardUptonPickman]
        | investigator <- investigators
        ]
      push $ DoStep 1 msg
      traverse_ placeLocation_ =<< getSetAsideCardsMatching (#location <> withTrait Vale)

      push $ RemoveAllCopiesOfEncounterCardFromGame (fromSets [StrikingFear])
      encounterSets <- getSetAsideCardsMatching (fromSets [TerrorOfTheVale, Nightgaunts])
      pushAll [ShuffleCardsIntoDeck Deck.EncounterDeck encounterSets, ShuffleEncounterDiscardBackIn]
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      richardUptonPickman <- selectJust $ assetIs Assets.richardUptonPickman
      gameModifier attrs richardUptonPickman RemoveFromGameInsteadOfDiscard
      pure a
    _ -> EnteringTheUnderworldV2 <$> lift (runMessage msg attrs)
