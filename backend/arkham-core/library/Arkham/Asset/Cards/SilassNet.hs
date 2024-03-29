module Arkham.Asset.Cards.SilassNet (
  silassNet,
  SilassNet (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype SilassNet = SilassNet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silassNet :: AssetCard SilassNet
silassNet =
  asset SilassNet Cards.silassNet

instance HasAbilities SilassNet where
  getAbilities (SilassNet attrs) = [restrictedAbility attrs 1 ControlsThis evadeAction_]

instance RunMessage SilassNet where
  runMessage msg a@(SilassNet attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ chooseEvadeEnemy iid (attrs.ability 1) #agility
      pure a
    SkillTestEnds iid (isAbilitySource attrs 1 -> True) -> do
      miid <- getSkillTestInvestigator
      when (Just iid == miid) do
        player <- getPlayer iid
        skills <- select $ skillControlledBy iid
        push
          $ chooseOne
            player
            [ Label
                "Return Silas's Net to your hand to return all of your committed skill cards to your hand instead of discarding them"
                $ ReturnToHand iid (toTarget attrs)
                : [ReturnToHand iid (toTarget skill) | skill <- skills]
            , Label "Do nothing" []
            ]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just eid -> do
          otherEnemies <- select $ enemyEngagedWith iid <> not_ (EnemyWithId eid)
          when (notNull otherEnemies) do
            player <- getPlayer iid
            push
              $ chooseOne
                player
                [ Label
                    "Automatically evade another enemy"
                    [chooseOrRunOne player [targetLabel enemy [EnemyEvaded iid enemy] | enemy <- otherEnemies]]
                , Label "Do not evade another enemy" []
                ]
        Nothing -> error "No target"
      pure a
    _ -> SilassNet <$> runMessage msg attrs
