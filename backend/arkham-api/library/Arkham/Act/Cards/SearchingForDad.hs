module Arkham.Act.Cards.SearchingForDad (searchingForDad) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.RedTideRising.Helpers
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers (getLeadsDeck)
import Arkham.Trait (Trait (Hideout, Suspect))

newtype SearchingForDad = SearchingForDad ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForDad :: ActCard SearchingForDad
searchingForDad = act (1, A) SearchingForDad Cards.searchingForDad Nothing

instance HasModifiersFor SearchingForDad where
  getModifiersFor (SearchingForDad a) =
    modifySelect a (EnemyWithTrait Suspect) [LoseVictory, victoryRequiresMysteriousPhoto]

instance HasAbilities SearchingForDad where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        ( wrapCriteria a
            $ Negate (oneOf [exists (EnemyWithTrait Suspect), exists (LocationWithTrait Hideout)])
            <> ScenarioDeckWithCard LeadsDeck
            <> exists wendyAdams
        )
        $ forced AnyWindow
    , restricted a 2 (Negate $ ScenarioDeckWithCard LeadsDeck) $ Objective $ forced AnyWindow
    ]
   where
    wrapCriteria a = if toResultDefault False a.meta then const Never else id

instance RunMessage SearchingForDad where
  runMessage msg a@(SearchingForDad attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      getLeadsDeck >>= \case
        [] -> pure ()
        (x : _) -> whenJustM getWendyAdams \wendy -> drawCardFrom wendy LeadsDeck x
      doStep 1 msg
      pure $ SearchingForDad $ attrs & metaL .~ toJSON True
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) ->
      pure $ SearchingForDad $ attrs & metaL .~ toJSON False
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> SearchingForDad <$> liftRunMessage msg attrs
