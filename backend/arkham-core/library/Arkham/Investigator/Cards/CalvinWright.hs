module Arkham.Investigator.Cards.CalvinWright
  ( calvinWright
  , CalvinWright(..)
  ) where

import Arkham.Prelude

import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype CalvinWright = CalvinWright InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

calvinWright :: InvestigatorCard CalvinWright
calvinWright = investigator
  CalvinWright
  Cards.calvinWright
  Stats
    { health = 6
    , sanity = 6
    , willpower = 0
    , intellect = 0
    , combat = 0
    , agility = 0
    }

instance HasModifiersFor CalvinWright where
  getModifiersFor (InvestigatorTarget iid) (CalvinWright a) | iid == toId a =
    do
      let
        horror = investigatorSanityDamage a
        damage = investigatorHealthDamage a
      pure
        $ toModifiers a
        $ [ SkillModifier SkillWillpower horror | horror > 0 ]
        <> [ SkillModifier SkillIntellect horror | horror > 0 ]
        <> [ SkillModifier SkillCombat damage | damage > 0 ]
        <> [ SkillModifier SkillAgility damage | damage > 0 ]
  getModifiersFor _ _ = pure []

instance HasAbilities CalvinWright where
  getAbilities (CalvinWright _) = []

instance HasTokenValue CalvinWright where
  getTokenValue iid ElderSign (CalvinWright attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign ZeroModifier
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage CalvinWright where
  runMessage msg i@(CalvinWright attrs) = case msg of
    ResolveToken _ ElderSign iid | iid == toId attrs -> do
      push
        $ chooseOne iid
        $ [ Label "Heal 1 Damage" [HealDamage (toTarget attrs) (toSource attrs) 1]
          | investigatorHealthDamage attrs > 0
          ]
        <> [ Label "Heal 1 Horror" [HealHorror (toTarget attrs) (toSource attrs) 1]
           | investigatorHealthDamage attrs > 0
           ]
        <> [ Label
             "Take 1 Direct Damage"
             [InvestigatorDirectDamage iid (toSource attrs) 1 0]
           , Label
             "Take 1 Direct Horror"
             [InvestigatorDirectDamage iid (toSource attrs) 0 1]
           , Label "Do not use elder sign ability" []
           ]
      pure i
    _ -> CalvinWright <$> runMessage msg attrs
