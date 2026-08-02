module Arkham.Treachery.Cards.ErodedFrieze (erodedFrieze) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Story (readStory)
import Arkham.Message.Lifted.Choose (chooseBeginSkillTest)
import Arkham.Story.Cards qualified as Stories
import Arkham.Token (Token (Resource))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ErodedFrieze = ErodedFrieze TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erodedFrieze :: TreacheryCard ErodedFrieze
erodedFrieze = treachery ErodedFrieze Cards.erodedFrieze

instance HasModifiersFor ErodedFrieze where
  -- "If Eroded Frieze would leave play, set it aside, out of play." We keep the
  -- card in play (CannotLeavePlay); this also covers the un-cancelable attach.
  getModifiersFor (ErodedFrieze attrs) = modifySelf attrs [CannotLeavePlay]

-- The card flips to its Glyph back once enough resources are on it; track that
-- in meta so the front-side action is no longer offered afterward.
flipped :: TreacheryAttrs -> Bool
flipped a = toResultDefault False a.meta

instance HasAbilities ErodedFrieze where
  getAbilities (ErodedFrieze a) =
    [skillTestAbility $ restricted a 1 OnSameLocation actionAbility | not (flipped a)]

instance RunMessage ErodedFrieze where
  runMessage msg t@(ErodedFrieze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- "Attach Eroded Frieze to your location. Cannot be canceled" — the
      -- un-cancelable part is declared on the card def; CannotLeavePlay above
      -- handles the "set it aside out of play" clause.
      withLocationOf iid (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Test [combat] or [intellect] (3); the player chooses which.
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#combat, #intellect] (Fixed 3)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- Put 1 resource on this card; if it now has 1 [per_investigator]
      -- resources, flip it and resolve its text.
      placeTokens (attrs.ability 1) attrs Resource 1
      requiredResources <- perPlayer 1
      when (attrs.resources + 1 >= requiredResources) $ flipOver iid attrs
      pure t
    Flip iid _ (isTarget attrs -> True) -> do
      -- "Flip it and resolve its text." The back (11664b) is a story card that
      -- translates the glyph and adds itself to the victory display. A treachery
      -- has no UI slot a story can replace, so the runner focuses the story card
      -- and waits for the player to click it — which is what lets them read it.
      readStory iid attrs Stories.erodedFriezeStory
      pure $ ErodedFrieze $ attrs & setMeta True
    _ -> ErodedFrieze <$> liftRunMessage msg attrs
