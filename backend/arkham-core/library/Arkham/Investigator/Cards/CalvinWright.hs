module Arkham.Investigator.Cards.CalvinWright (
  calvinWright,
  CalvinWright (..),
) where

import Arkham.Prelude

import Arkham.Game.Helpers
import Arkham.Helpers.Investigator
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner

newtype CalvinWright = CalvinWright InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

calvinWright :: InvestigatorCard CalvinWright
calvinWright =
  investigator CalvinWright Cards.calvinWright
    $ Stats {health = 6, sanity = 6, willpower = 0, intellect = 0, combat = 0, agility = 0}

instance HasModifiersFor CalvinWright where
  getModifiersFor target (CalvinWright a) | a `isTarget` target = do
    let horror = a.sanityDamage
    let damage = a.healthDamage
    pure
      $ toModifiers a
      $ [SkillModifier skill horror | horror > 0, skill <- [#willpower, #intellect]]
      <> [SkillModifier skill damage | damage > 0, skill <- [#combat, #agility]]
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue CalvinWright where
  getChaosTokenValue iid ElderSign (CalvinWright attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign ZeroModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage CalvinWright where
  runMessage msg i@(CalvinWright attrs) = case msg of
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      mHealHorror <- getHealHorrorMessage attrs 1 iid
      canHealDamage <- canHaveDamageHealed attrs iid
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [Label "Heal 1 Damage" [HealDamage (toTarget attrs) (toSource attrs) 1] | canHealDamage]
        <> [Label "Heal 1 Horror" [healHorror] | healHorror <- toList mHealHorror]
        <> [ Label "Take 1 Direct Damage" [directDamage iid attrs 1]
           , Label "Take 1 Direct Horror" [directHorror iid attrs 1]
           , Label "Do not use elder sign ability" []
           ]
      pure i
    _ -> CalvinWright <$> runMessage msg attrs
