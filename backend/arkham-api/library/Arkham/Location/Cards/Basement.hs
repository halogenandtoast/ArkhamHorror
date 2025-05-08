module Arkham.Location.Cards.Basement (basement) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Name
import Arkham.Projection
import Arkham.Trait (Trait (Humanoid))

newtype Basement = Basement LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

basement :: LocationCard Basement
basement = location Basement Cards.basement 4 (PerPlayer 1)

instance HasAbilities Basement where
  getAbilities (Basement attrs) =
    extendRevealed
      attrs
      [ skillTestAbility $ restricted attrs 1 Here actionAbility
      , restricted
          attrs
          2
          ( exists (assetIs Assets.tomeOfRituals)
              <> exists (investigatorAt attrs <> InvestigatorWithAnyClues)
          )
          $ freeReaction (EnemyDefeated #after You ByAny (enemyAt attrs))
      ]

instance RunMessage Basement where
  runMessage msg l@(Basement attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 1)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      humanoids <- select $ EnemyWithTrait Humanoid <> NotEnemy (enemyAt $ toId attrs)
      chooseOrRunOneM iid $ targets humanoids (`moveToward` attrs)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      iids <- selectWithField InvestigatorClues $ investigatorAt attrs <> InvestigatorWithAnyClues
      tomeOfRituals <- selectOne $ assetIs Assets.tomeOfRituals

      unless (null iids || isNothing tomeOfRituals) $ do
        named <- traverse (\(iid', x) -> (,x) <$> field InvestigatorName iid') iids
        chooseAmounts
          iid
          "number of clues to move to Tome of Rituals"
          (MinAmountTarget 0)
          (map (\(name, x) -> (toTitle name, (0, x))) named)
          attrs
      pure l
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      named <- selectWithField InvestigatorName UneliminatedInvestigator
      tomeOfRituals <- selectJust $ assetIs Assets.tomeOfRituals

      for_ named \(iid', name) -> do
        let amount = getChoiceAmount (toTitle name) choices
        when (amount > 0) do
          moveTokens (attrs.ability 2) iid' tomeOfRituals #clue amount
      pure l
    _ -> Basement <$> liftRunMessage msg attrs
