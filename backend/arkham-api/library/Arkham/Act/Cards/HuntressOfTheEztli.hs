module Arkham.Act.Cards.HuntressOfTheEztli (huntressOfTheEztli) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Ruins))

newtype HuntressOfTheEztli = HuntressOfTheEztli ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntressOfTheEztli :: ActCard HuntressOfTheEztli
huntressOfTheEztli = act (2, A) HuntressOfTheEztli Cards.huntressOfTheEztli Nothing

instance HasAbilities HuntressOfTheEztli where
  getAbilities = actAbilities \x ->
    [ mkAbility x 1 $ Objective $ forced $ EnemyDefeated #after Anyone ByAny "Ichtaca"
    , restricted x 2 (exists $ "Ichtaca" <> EnemyWithClues (AtLeast $ PerPlayer 1))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage HuntressOfTheEztli where
  runMessage msg a@(HuntressOfTheEztli attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      ichtacaDefeated <- selectAny $ DefeatedEnemy "Ichtaca"
      ruins <- getSetAsideCardsMatching $ CardWithTrait Ruins
      if ichtacaDefeated
        then do
          investigators <- getInvestigators
          alejandroVela <- fetchCard $ SetAsideCardMatch "Alejandro Vela"
          remember YouFoughtWithIchtaca
          leadChooseOneM $ withI18n do
            nameVar Assets.alejandroVela $ questionLabeled' "takeControlOf"
            questionLabeledCard Assets.alejandroVela
            portraits investigators (`takeControlOfSetAsideAsset` alejandroVela)
          addChaosToken Tablet
          shuffleCardsIntoDeck ExplorationDeck ruins
          advanceToAct attrs Acts.theGuardedRuins A
        else do
          itchtaca <- selectJust $ enemyIs Enemies.ichtaca
          addToVictory itchtaca
          remember IchtachaIsLeadingTheWay
          addChaosToken Cultist
          shuffleCardsIntoDeck ExplorationDeck ruins
          advanceToAct attrs Acts.searchForTheRuins A
      pure a
    _ -> HuntressOfTheEztli <$> liftRunMessage msg attrs
