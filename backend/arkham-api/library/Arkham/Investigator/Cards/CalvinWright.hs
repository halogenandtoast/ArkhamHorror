module Arkham.Investigator.Cards.CalvinWright (calvinWright) where

import Arkham.Helpers.Investigator
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted

newtype CalvinWright = CalvinWright InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

calvinWright :: InvestigatorCard CalvinWright
calvinWright =
  investigator CalvinWright Cards.calvinWright
    $ Stats {health = 6, sanity = 6, willpower = 0, intellect = 0, combat = 0, agility = 0}

instance HasModifiersFor CalvinWright where
  getModifiersFor (CalvinWright a) = do
    let horror = a.sanityDamage
    let damage = a.healthDamage
    modifySelf a
      $ [SkillModifier skill horror | horror > 0, skill <- [#willpower, #intellect]]
      <> [SkillModifier skill damage | damage > 0, skill <- [#combat, #agility]]

instance HasChaosTokenValue CalvinWright where
  getChaosTokenValue iid ElderSign (CalvinWright attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign ZeroModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage CalvinWright where
  runMessage msg i@(CalvinWright attrs) = runQueueT $ case msg of
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      canHealHorror <- canHaveHorrorHealed attrs iid
      canHealDamage <- canHaveDamageHealed attrs iid
      chooseOne iid
        $ [Label "$label.healDamage count=i:1" [HealDamage (toTarget attrs) (toSource attrs) 1] | canHealDamage]
        <> [Label "$label.healHorror count=i:1" [HealHorror (toTarget attrs) (toSource attrs) 1] | canHealHorror]
        <> [ Label "$label.takeOneDirectDamage" [Msg.directDamage iid attrs 1]
           , Label "$label.takeOneDirectHorror" [Msg.directHorror iid attrs 1]
           , Label "$label.doNotUseElderSign" []
           ]
      pure i
    _ -> CalvinWright <$> liftRunMessage msg attrs
