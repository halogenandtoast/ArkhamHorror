module Arkham.Location.Cards.TreacherousPath (treacherousPath, TreacherousPath (..)) where

import Arkham.Ability
import Arkham.Helpers.Window (getRevealedChaosTokens)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TreacherousPath = TreacherousPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPath :: LocationCard TreacherousPath
treacherousPath = symbolLabel $ location TreacherousPath Cards.treacherousPath 2 (PerPlayer 1)

instance HasAbilities TreacherousPath where
  getAbilities (TreacherousPath attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (Here <> DuringSkillTest AnySkillTest)
      $ forced
      $ RevealChaosToken #after You #frost

instance RunMessage TreacherousPath where
  runMessage msg l@(TreacherousPath attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getRevealedChaosTokens -> tokens) _ -> do
      let n = count ((== #frost) . (.face)) tokens
      repeated n do
        chooseOneM iid do
          labeled "Take damage" $ assignDamage iid (attrs.ability 1) 1
          labeled "Take horror" $ assignHorror iid (attrs.ability 1) 1

      pure l
    _ -> TreacherousPath <$> liftRunMessage msg attrs
