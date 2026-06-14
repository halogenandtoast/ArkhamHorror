module Arkham.Act.Cards.StepsOfGiants (stepsOfGiants) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Glyph, Omen))

newtype StepsOfGiants = StepsOfGiants ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfGiants :: ActCard StepsOfGiants
stepsOfGiants = act (1, A) StepsOfGiants Cards.stepsOfGiants Nothing

instance HasAbilities StepsOfGiants where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ InvestigatorWithClues $ atLeast 1)
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
    , restricted a 2 (exists $ locationIs Locations.greatLiftActive)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage StepsOfGiants where
  runMessage msg a@(StepsOfGiants attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- getPlayerCount
      discardTopOfEncounterDeckAndHandle iid attrs (if n == 1 then 8 else 5) attrs
      pure a
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      let glyphs = filterCards (CardWithTrait Glyph) cards
      let omens = filterCards (CardWithTrait Omen) cards
      focusCards (glyphs <> omens) do
        for_ (take 1 glyphs) (drawCard iid)
        for_ omens (drawCard iid)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      greatLift <- selectJust $ locationIs Locations.greatLiftActive
      tyrant <- getSetAsideCard Enemies.colossalTyrant
      tyrantId <- createEnemyAt tyrant greatLift
      investigators <- select $ investigatorAt greatLift
      lead <- getLead
      sid <- getRandom
      chooseOrRunOneM lead $ targets investigators \iid -> do
        onSucceedByEffect sid AnyValue attrs tyrantId $ exhaustThis tyrantId
        beginSkillTest sid iid attrs tyrantId #agility (Fixed 3)
      advanceActDeck attrs
      pure a
    _ -> StepsOfGiants <$> liftRunMessage msg attrs
