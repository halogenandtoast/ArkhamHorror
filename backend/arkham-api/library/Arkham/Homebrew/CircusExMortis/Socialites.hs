{- | The five Bacchanalia Socialite story assets print one ability between them:

@[action]: Parley. Test \<skill\> (\<difficulty\>). \<vice clause\> If you succeed, take
control of 1 clue on \<name\>. If you succeed by 2 or more, take control of 1 additional
clue or discover 1 clue from \<name\>'s location. If you fail, \<penalty\>.@

Only the skill, the difficulty clause and the failure penalty differ, so the ability and
the success rider live here and each card supplies its own three lines.
-}
module Arkham.Homebrew.CircusExMortis.Socialites where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (getCanDiscoverClues)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.CircusExMortis.Helpers (Vice, hasVice, scenarioI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.SkillType (SkillType)

socialiteAbilities :: AssetAttrs -> [Ability]
socialiteAbilities a = [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

{- | "Test \<skill\> (\<difficulty\>)", with @riders@ applied to the test as modifiers.
The vice clauses are worded as difficulty adjustments, so they ride as 'Difficulty'
modifiers rather than changing the printed base: a 'SetDifficulty' pre-modifier then
still replaces the printed number, and the panel shows the adjustment.
-}
socialiteParley
  :: ReverseQueue m
  => AssetAttrs -> InvestigatorId -> SkillType -> Int -> [ModifierType] -> m ()
socialiteParley attrs iid sType difficulty riders = do
  sid <- getRandom
  for_ riders $ skillTestModifier sid (attrs.ability 1) sid
  parley sid iid (attrs.ability 1) attrs sType (Fixed difficulty)

{- | "Test \<skill\> (5). If you have "a vice for \<vice\>", this test gets -2 difficulty."
Four of the five read this way; Phillip Hutchins is the exception.
-}
socialiteViceParley
  :: ReverseQueue m => AssetAttrs -> InvestigatorId -> SkillType -> Vice -> m ()
socialiteViceParley attrs iid sType vice = do
  discounted <- hasVice iid vice
  socialiteParley attrs iid sType 5 [Difficulty (-2) | discounted]

{- | "If you succeed, take control of 1 clue on this card. If you succeed by 2 or more,
take control of 1 additional clue or discover 1 clue from this card's location."

The additional clue is only offered when one is still on the card after the first is
taken, and the discover only when the location has a clue this investigator may take.
-}
socialiteSuccess :: ReverseQueue m => AssetAttrs -> InvestigatorId -> Int -> m ()
socialiteSuccess attrs iid n = do
  modifiers <- getModifiers iid
  let canTakeControl = CannotTakeControlOfClues `notElem` modifiers
  let taken = if canTakeControl && attrs.token #clue > 0 then 1 else 0
  when (taken > 0) $ moveTokens (attrs.ability 1) attrs iid #clue 1
  when (n >= 2) do
    mlid <- getLocationOf attrs
    canDiscover <- maybe (pure False) (getCanDiscoverClues NotInvestigate iid) mlid
    let canTakeAnother = canTakeControl && attrs.token #clue - taken > 0
    when (canTakeAnother || canDiscover) do
      chooseOneM iid $ scenarioI18n "bacchanalia" $ scope "socialite" do
        when canTakeAnother do
          labeled "takeAdditionalClue" $ moveTokens (attrs.ability 1) attrs iid #clue 1
        for_ mlid \lid -> when canDiscover do
          labeled "discoverClue" $ discoverAt NotInvestigate iid (attrs.ability 1) 1 lid
