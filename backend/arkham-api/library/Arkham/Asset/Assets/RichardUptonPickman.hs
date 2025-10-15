module Arkham.Asset.Assets.RichardUptonPickman (richardUptonPickman) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Helpers.Modifiers hiding (skillTestModifiers)
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ghoul))

newtype RichardUptonPickman = RichardUptonPickman AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

richardUptonPickman :: AssetCard RichardUptonPickman
richardUptonPickman = assetWith RichardUptonPickman Cards.richardUptonPickman (healthL ?~ 3)

instance HasAbilities RichardUptonPickman where
  getAbilities (RichardUptonPickman a) =
    [ controlled a 1 (exists $ EnemyAt YourLocation <> withTrait Ghoul) $ FastAbility (exhaust a)
    , controlled_ a 2 $ fightAction (exhaust a)
    ]

instance RunMessage RichardUptonPickman where
  runMessage msg a@(RichardUptonPickman attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ghouls <- selectWithField EnemyCard (enemyAtLocationWith iid <> withTrait Ghoul)
      chooseOneM iid $ for_ ghouls \(ghoul, card) -> targeting ghoul $ placeUnderneath attrs (only card)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [BaseSkillOf #combat 5, NoStandardDamage]
      chooseFightEnemyEdit sid iid (attrs.ability 1) (setTarget attrs)
      pure a
    Successful (Action.Fight, EnemyTarget eid) _iid _ (isTarget attrs -> True) _ -> do
      let n = count isEncounterCard attrs.cardsUnderneath
      attackEnemyDamage (attrs.ability 2) n eid
      pure a
    _ -> RichardUptonPickman <$> liftRunMessage msg attrs
