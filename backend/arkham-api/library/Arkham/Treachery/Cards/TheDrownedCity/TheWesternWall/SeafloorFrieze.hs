module Arkham.Treachery.Cards.TheDrownedCity.TheWesternWall.SeafloorFrieze (seafloorFrieze) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Story (readStory)
import Arkham.Story.CardDefs.TheDrownedCity.TheWesternWall qualified as Stories
import Arkham.Target
import Arkham.Treachery.CardDefs.TheDrownedCity.TheWesternWall qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SeafloorFrieze = SeafloorFrieze TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seafloorFrieze :: TreacheryCard SeafloorFrieze
seafloorFrieze = treachery SeafloorFrieze Cards.seafloorFrieze

instance HasModifiersFor SeafloorFrieze where
  getModifiersFor (SeafloorFrieze attrs) = modifySelf attrs [CannotLeavePlay]

instance HasAbilities SeafloorFrieze where
  getAbilities (SeafloorFrieze a) = [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

-- Both tests always happen; only the flip is conditional on passing both. The
-- stage is tracked in meta rather than read off the skill test's type, which
-- ChangeSkillTestType (Money Talks, the Carnevale masks) rewrites in place.
friezeStep :: ReverseQueue m => InvestigatorId -> Bool -> TreacheryAttrs -> m TreacheryAttrs
friezeStep iid passed attrs = case toResultDefault (0 :: Int, False) attrs.meta of
  (1, _) -> do
    sid <- getRandom
    beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
    pure $ setMeta (2 :: Int, passed) attrs
  (2, passedFirst) -> do
    when (passedFirst && passed) $ flipOver iid attrs
    pure $ setMeta (0 :: Int, False) attrs
  _ -> pure attrs

instance RunMessage SeafloorFrieze where
  runMessage msg t@(SeafloorFrieze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Revelation cannot be canceled; attach to your location. (CannotLeavePlay handled above.)
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 2)
      pure . SeafloorFrieze $ setMeta (1 :: Int, False) attrs
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) Initiator {} _ _ ->
      SeafloorFrieze <$> friezeStep iid True attrs
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) Initiator {} _ _ ->
      SeafloorFrieze <$> friezeStep iid False attrs
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.seafloorFrieze
      pure t
    _ -> SeafloorFrieze <$> liftRunMessage msg attrs
