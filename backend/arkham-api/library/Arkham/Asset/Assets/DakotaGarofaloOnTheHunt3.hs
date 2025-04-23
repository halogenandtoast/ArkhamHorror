module Arkham.Asset.Assets.DakotaGarofaloOnTheHunt3 (dakotaGarofaloOnTheHunt3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGetsMaybe)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait (Trait (Creature, Monster))

newtype DakotaGarofaloOnTheHunt3 = DakotaGarofaloOnTheHunt3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dakotaGarofaloOnTheHunt3 :: AssetCard DakotaGarofaloOnTheHunt3
dakotaGarofaloOnTheHunt3 = ally DakotaGarofaloOnTheHunt3 Cards.dakotaGarofaloOnTheHunt3 (2, 2)

instance HasModifiersFor DakotaGarofaloOnTheHunt3 where
  getModifiersFor (DakotaGarofaloOnTheHunt3 a) = controllerGetsMaybe a \iid -> do
    liftGuardM $ selectAny $ enemyAtLocationWith iid <> hasAnyTrait [Creature, Monster]
    pure [SkillModifier #willpower 1, SkillModifier #combat 1]

instance HasAbilities DakotaGarofaloOnTheHunt3 where
  getAbilities (DakotaGarofaloOnTheHunt3 x) =
    [ controlled
        x
        1
        ( exists
            $ oneOf
              [ at_ YourLocation <> CanEngageEnemy (x.ability 1)
              , at_ (AccessibleFrom YourLocation <> CanEnterLocation You)
              ]
        )
        $ FastAbility (exhaust x)
    ]

instance RunMessage DakotaGarofaloOnTheHunt3 where
  runMessage msg a@(DakotaGarofaloOnTheHunt3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select
          $ oneOf
            [ enemyAtLocationWith iid <> CanEngageEnemy (attrs.ability 1)
            , EnemyAt (AccessibleFrom (locationWithInvestigator iid) <> CanEnterLocation (InvestigatorWithId iid))
            ]
      chooseTargetM iid enemies (handleTarget iid attrs)
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      withLocationOf iid \lid -> do
        melid <- field EnemyLocation eid
        if Just lid == melid
          then whenM (eid <=~> CanEngageEnemy (attrs.ability 1)) $ engageEnemy iid eid
          else do
            for_ melid $ moveTo (attrs.ability 1) iid
            doStep 2 msg
      pure a
    DoStep 2 (HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid)) -> do
      whenM (eid <=~> CanEngageEnemy (attrs.ability 1)) $ engageEnemy iid eid
      pure a
    _ -> DakotaGarofaloOnTheHunt3 <$> liftRunMessage msg attrs
