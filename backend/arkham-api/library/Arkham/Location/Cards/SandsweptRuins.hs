module Arkham.Location.Cards.SandsweptRuins (sandsweptRuins) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscardN)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype SandsweptRuins = SandsweptRuins LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sandsweptRuins :: LocationCard SandsweptRuins
sandsweptRuins = location SandsweptRuins Cards.sandsweptRuins 3 (PerPlayer 1)

instance HasModifiersFor SandsweptRuins where
  getModifiersFor (SandsweptRuins a) = do
    getSkillTestInvestigator >>= traverse_ \iid -> do
      investigating <- isInvestigating iid a.id
      when investigating do
        handSize <- fieldMap InvestigatorHand length iid
        modifySelfWhen a (handSize >= 5) [ShroudModifier 3]

instance HasAbilities SandsweptRuins where
  getAbilities (SandsweptRuins a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> youExist (HandWith $ LengthIs $ atLeast 8))
      doubleActionAbility

instance RunMessage SandsweptRuins where
  runMessage msg l@(SandsweptRuins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscardN iid (attrs.ability 1) 3
      removeStrengthOfTheAbyss 1
      pure l
    _ -> SandsweptRuins <$> liftRunMessage msg attrs
