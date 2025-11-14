module Arkham.Asset.Assets.IsamaraOrdonezLoungeSingerCrew (isamaraOrdonezLoungeSingerCrew) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Casino, Restricted))

newtype IsamaraOrdonezLoungeSingerCrew = IsamaraOrdonezLoungeSingerCrew AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isamaraOrdonezLoungeSingerCrew :: AssetCard IsamaraOrdonezLoungeSingerCrew
isamaraOrdonezLoungeSingerCrew =
  allyWith IsamaraOrdonezLoungeSingerCrew Cards.isamaraOrdonezLoungeSingerCrew (1, 3) noSlots

instance HasAbilities IsamaraOrdonezLoungeSingerCrew where
  getAbilities (IsamaraOrdonezLoungeSingerCrew a) =
    [ restricted a 1 (OnSameLocation <> youExist (not_ $ HasMatchingAsset (be a))) parleyAction_
    , groupLimit PerGame
        $ controlled
          a
          2
          (exists $ EnemyAt YourLocation <> EnemyWithTrait Casino <> EnemyCanBeEvadedBy (a.ability 2))
          parleyAction_
    ]

instance RunMessage IsamaraOrdonezLoungeSingerCrew where
  runMessage msg a@(IsamaraOrdonezLoungeSingerCrew attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeControlOfAsset iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectEach
        ( EnemyAt (locationWithInvestigator iid)
            <> EnemyWithTrait Casino
            <> EnemyCanBeEvadedBy (attrs.ability 2)
        )
        (automaticallyEvadeEnemy iid)
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      withLocationOf iid \loc -> do
        whenMatch loc (LocationWithTrait Restricted) do
          remember IsamaraMesmerizedTheGuardsWithHerSong
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      loseActions iid (attrs.ability 2) 2
      pure a
    _ -> IsamaraOrdonezLoungeSingerCrew <$> liftRunMessage msg attrs
