module Arkham.Treachery.Cards.PrismaticPhenomenon (prismaticPhenomenon, PrismaticPhenomenon (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Helpers.SkillTest (getSkillTestTarget, withSkillTest)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PrismaticPhenomenon = PrismaticPhenomenon TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prismaticPhenomenon :: TreacheryCard PrismaticPhenomenon
prismaticPhenomenon = treachery PrismaticPhenomenon Cards.prismaticPhenomenon

instance HasModifiersFor PrismaticPhenomenon where
  getModifiersFor (PrismaticPhenomenon attrs) =
    inThreatAreaGets
      attrs
      [ AdditionalActionCostOf (FirstOneOfPerformed [#draw, #resource, #play]) 1
      ]

instance HasAbilities PrismaticPhenomenon where
  getAbilities (PrismaticPhenomenon a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ SkillTestResult #when You #investigating #success
    ]

instance RunMessage PrismaticPhenomenon where
  runMessage msg t@(PrismaticPhenomenon attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      getSkillTestTarget >>= \case
        Nothing -> error "invalid target"
        Just target -> withSkillTest \sid ->
          skillTestModifier sid (attrs.ability 1) target (AlternateSuccessfullInvestigation $ toTarget attrs)
      pure t
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> PrismaticPhenomenon <$> liftRunMessage msg attrs
