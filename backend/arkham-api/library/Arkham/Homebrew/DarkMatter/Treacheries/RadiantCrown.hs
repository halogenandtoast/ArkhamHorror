module Arkham.Homebrew.DarkMatter.Treacheries.RadiantCrown (radiantCrown) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RadiantCrown = RadiantCrown TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

radiantCrown :: TreacheryCard RadiantCrown
radiantCrown = treachery RadiantCrown Cards.radiantCrown

instance HasModifiersFor RadiantCrown where
  getModifiersFor (RadiantCrown a) = case a.placement of
    NextToAgenda ->
      getSkillTest >>= \case
        Nothing -> pure ()
        Just st -> modified_ a (SkillTestTarget st.id) [Difficulty 1]
    _ -> pure ()

instance HasAbilities RadiantCrown where
  getAbilities (RadiantCrown a) =
    [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage RadiantCrown where
  runMessage msg t@(RadiantCrown attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      gainSurge attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> RadiantCrown <$> liftRunMessage msg attrs
