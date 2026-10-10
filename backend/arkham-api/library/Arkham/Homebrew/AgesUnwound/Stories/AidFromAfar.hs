module Arkham.Homebrew.AgesUnwound.Stories.AidFromAfar (aidFromAfar) where

import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Log (remembered)
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDown.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (incrementRecordCount, remember)
import Arkham.Story.Import.Lifted
import Arkham.Strategy
import Arkham.Trait (Trait (Elite))

newtype AidFromAfar = AidFromAfar StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Drawn by the @[action]@ on whichever agenda is current -- printed on agenda
1a, granted to agendas 2a and 3a by agenda 1b.
-}
aidFromAfar :: StoryCard AidFromAfar
aidFromAfar = story AidFromAfar Cards.aidFromAfar

{- | "Mark 1 Strange Assistance in your Campaign Log. Choose one effect you have
not yet chosen this scenario, then set this card aside, out of play."

"Not yet chosen this scenario" is three scenario-log keys rather than a count:
the effects are not interchangeable, so which ones are spent has to be
remembered individually.

Setting the card aside needs no message. The agenda reads it out of the
set-aside pool without obtaining it, and the story entity is removed once its
resolution finishes ('storyRemoveAfterResolution'), so the card never leaves
where it started and the action can be taken again.
-}
instance RunMessage AidFromAfar where
  runMessage msg s@(AidFromAfar attrs) = runQueueT $ scenarioI18n $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      incrementRecordCount StrangeAssistance 1

      unchosen <- filterM (fmap not . remembered) aidFromAfarEffects
      let offered k = k `elem` unchosen

      perInvestigator <- perPlayer 1
      lid <- getJustLocation iid
      nonElite <- select $ EnemyAt (LocationWithId lid) <> not_ (EnemyWithTrait Elite)
      elite <- select $ EnemyAt (LocationWithId lid) <> EnemyWithTrait Elite

      -- Nothing left to choose only after all three have been taken; the mark in
      -- the campaign log still happens, which is what the card says.
      unless (null unchosen) $ chooseOneM iid $ scope "aidFromAfar" do
        -- "Each investigator may search the top 9 cards of their deck for a card
        -- and draw it."
        when (offered aidFromAfarSearchedTheirDecks) $ labeled "searchYourDecks" do
          remember aidFromAfarSearchedTheirDecks
          eachInvestigator \iid' ->
            search iid' attrs iid' [fromTopOfDeck 9] #any (DrawFound iid' 1)

        -- "Discover 1[per_investigator] clues from your location."
        when (offered aidFromAfarFoundAnInscription)
          $ countVar perInvestigator
          $ labeled "discoverClues" do
            remember aidFromAfarFoundAnInscription
            discoverAtYourLocation NotInvestigate iid attrs perInvestigator

        {- "Defeat a non-[[Elite]] enemy at your location, or deal
        1[per_investigator] damage to an [[Elite]] enemy at your location and
        exhaust it." -}
        when (offered aidFromAfarCalledInGunfire)
          $ countVar perInvestigator
          $ labeledValidate (notNull nonElite || notNull elite) "callInGunfire" do
            remember aidFromAfarCalledInGunfire
            chooseOneM iid do
              targets nonElite \eid -> defeatEnemy eid iid attrs
              targets elite \eid -> do
                nonAttackEnemyDamage (Just iid) attrs perInvestigator eid
                exhaustEnemy attrs eid
      pure s
    _ -> AidFromAfar <$> liftRunMessage msg attrs
