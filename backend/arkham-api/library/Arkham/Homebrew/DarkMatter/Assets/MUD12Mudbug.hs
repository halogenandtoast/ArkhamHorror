module Arkham.Homebrew.DarkMatter.Assets.MUD12Mudbug (muD12Mudbug) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message (pattern MovedDamage)
import Arkham.Message.Lifted.Choose

newtype MUD12Mudbug = MUD12Mudbug AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

muD12Mudbug :: AssetCard MUD12Mudbug
-- No printed sanity: a robot takes damage only.
muD12Mudbug = ally MUD12Mudbug Cards.muD12Mudbug (3, 0)

{- | "[action]: Move 2 damage between investigators, [[Ally]] assets, and enemies
at your location. (Group limit once per game.)"

Damage moves one point at a time so each point may go somewhere different;
'MovedDamage' wants the donor as a 'Source' and the recipient as a 'Target', so
both sides are carried as (Target, Source) pairs and matched up by target.
-}
instance HasAbilities MUD12Mudbug where
  getAbilities (MUD12Mudbug a) =
    [groupLimit PerGame $ controlled_ a 1 actionAbility]

instance RunMessage MUD12Mudbug where
  runMessage msg a@(MUD12Mudbug attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      doStep 2 msg
      pure a
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      let here = locationWithAsset attrs.id
      damagedInvestigators <- select $ InvestigatorAt here <> InvestigatorWithAnyDamage
      damagedAllies <- select $ #ally <> AssetAt here <> AssetWithDamage
      damagedEnemies <- select $ EnemyAt here <> EnemyWithDamage (atLeast 1)
      allInvestigators <- select $ InvestigatorAt here
      allAllies <- select $ #ally <> AssetAt here
      allEnemies <- select $ EnemyAt here
      let donors =
            map (\i -> (toTarget i, toSource i)) damagedInvestigators
              <> map (\x -> (toTarget x, toSource x)) damagedAllies
              <> map (\e -> (toTarget e, toSource e)) damagedEnemies
          recipients =
            map toTarget allInvestigators <> map toTarget allAllies <> map toTarget allEnemies
      unless (null donors) do
        chooseOneM iid $ targets (map fst donors) \donor ->
          for_ (lookup donor donors) \donorSource ->
            chooseOneM iid $ targets (filter (/= donor) recipients) \recipient -> do
              push $ MovedDamage (attrs.ability 1) donorSource recipient 1
              doStep (n - 1) msg'
      pure a
    _ -> MUD12Mudbug <$> liftRunMessage msg attrs
