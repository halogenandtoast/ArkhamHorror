module Arkham.Asset.Cards.SwordCane (swordCane, SwordCane (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype SwordCane = SwordCane AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
      let source = attrs.ability 2
      fightableEnemies <- select $ CanFightEnemy source
      evadeableEnemies <- select $ CanEvadeEnemy source
      player <- getPlayer iid
      let doEvade = chooseOne player [SkillLabel sk [chooseEvadeEnemy iid source sk] | sk <- [#willpower, #agility]]

      fightChoices <- for [#willpower, #combat] \sk -> do
        chooseFight <- toMessage . withSkillType sk <$> mkChooseFight iid source
        pure $ SkillLabel sk [chooseFight]

      let doFight = chooseOne player fightChoices

      case (fightableEnemies, evadeableEnemies) of
        ([], []) -> error "invalid call"
        ([], _ : _) -> push doEvade
        (_ : _, []) -> push doFight
        (_ : _, _ : _) -> do
          push
            $ chooseOne
              player
              [ Label "Evade" [doEvade]
              , Label "Fight" [doFight]
              ]
      pure a
    _ -> SwordCane <$> runMessage msg attrs
