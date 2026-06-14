module Arkham.Treachery.Cards.ErodedFrieze (erodedFrieze) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Message.Lifted.Choose (chooseBeginSkillTest)
import Arkham.Message.Lifted.Log (record)
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

instance HasAbilities ErodedFrieze where
  getAbilities (ErodedFrieze a) = [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage ErodedFrieze where
  runMessage msg t@(ErodedFrieze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Attach to your location. Cannot be canceled (revelations are not
      -- cancelable here, so no extra handling is required); CannotLeavePlay
      -- above handles the "set it aside out of play" clause.
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
      -- Back side (story 11664b): "You discover this glyph." Record "Darkness"
      -- under rune_e on the glyph record; this glyph has been translated. Add
      -- this card to the victory display.
      -- TODO: once 11664b is implemented as the proper story/back side, resolve
      -- its flavor/story text here (likely via readStory) in addition to the
      -- glyph translation below.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_e" :: Text, "Darkness" :: Text)
      addToVictory iid attrs
      pure t
    _ -> ErodedFrieze <$> liftRunMessage msg attrs
