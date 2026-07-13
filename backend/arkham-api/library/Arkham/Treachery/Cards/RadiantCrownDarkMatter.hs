module Arkham.Treachery.Cards.RadiantCrownDarkMatter (radiantCrownDarkMatter) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RadiantCrownDarkMatter = RadiantCrownDarkMatter TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

radiantCrownDarkMatter :: TreacheryCard RadiantCrownDarkMatter
radiantCrownDarkMatter = treachery RadiantCrownDarkMatter Cards.radiantCrownDarkMatter

instance HasModifiersFor RadiantCrownDarkMatter where
  getModifiersFor (RadiantCrownDarkMatter a) = case a.placement of
    NextToAgenda ->
      getSkillTest >>= \case
        Nothing -> pure ()
        Just st -> modified_ a (SkillTestTarget st.id) [Difficulty 1]
    _ -> pure ()

instance HasAbilities RadiantCrownDarkMatter where
  getAbilities (RadiantCrownDarkMatter a) =
    [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage RadiantCrownDarkMatter where
  runMessage msg t@(RadiantCrownDarkMatter attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      gainSurge attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> RadiantCrownDarkMatter <$> liftRunMessage msg attrs
