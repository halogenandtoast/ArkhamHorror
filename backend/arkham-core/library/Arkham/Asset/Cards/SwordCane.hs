module Arkham.Asset.Cards.SwordCane (
  swordCane,
  SwordCane (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype SwordCane = SwordCane AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

swordCane :: AssetCard SwordCane
swordCane = asset SwordCane Cards.swordCane

instance HasAbilities SwordCane where
  getAbilities (SwordCane x) =
    [ controlledAbility
        x
        1
        (exists $ oneOf [CanEvadeEnemy (toAbilitySource x 2), CanFightEnemy (toAbilitySource x 2)])
        $ freeReaction
        $ AssetEntersPlay #after
        $ AssetWithId (toId x)
    , displayAsAction $ restrictedAbility x 2 ControlsThis $ fightAction $ exhaust x
    , displayAsAction $ restrictedAbility x 2 ControlsThis $ evadeAction $ exhaust x
    ]

instance RunMessage SwordCane where
  runMessage msg a@(SwordCane attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' payments -> do
      runMessage (UseCardAbility iid (toSource attrs) 2 windows' payments) a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      fightableEnemies <- selectList $ CanFightEnemy (toSource attrs)
      evadeableEnemies <- selectList $ CanEvadeEnemy (toSource attrs)
      player <- getPlayer iid
      let doEvade = chooseOne player [SkillLabel sk [chooseEvadeEnemy iid attrs sk] | sk <- [#willpower, #agility]]
      let doFight = chooseOne player [SkillLabel sk [chooseFightEnemy iid attrs sk] | sk <- [#willpower, #combat]]

      case (fightableEnemies, evadeableEnemies) of
        ([], []) -> error "invalid call"
        ([], (_ : _)) -> push doEvade
        ((_ : _), []) -> push doFight
        ((_ : _), (_ : _)) -> do
          push
            $ chooseOne
              player
              [ Label "Evade" [doEvade]
              , Label "Fight" [doFight]
              ]
      pure a
    _ -> SwordCane <$> runMessage msg attrs
