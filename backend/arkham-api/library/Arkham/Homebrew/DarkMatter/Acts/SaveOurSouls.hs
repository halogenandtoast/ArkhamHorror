module Arkham.Homebrew.DarkMatter.Acts.SaveOurSouls (saveOurSouls) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (
  crewForEvidence,
  getRemovedCrew,
  getScanningDeck,
  isImitationToken,
 )
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern ScanningDeck)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait (Trait (Crew))

newtype SaveOurSouls = SaveOurSouls ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saveOurSouls :: ActCard SaveOurSouls
saveOurSouls = act (2, A) SaveOurSouls Cards.saveOurSouls Nothing

-- "Objective - If each undefeated investigator has resigned: (-> R1)."
instance HasAbilities SaveOurSouls where
  getAbilities (SaveOurSouls a) =
    [ onlyOnce
        $ restricted a 1 (not_ $ exists $ UneliminatedInvestigator <> not_ ResignedInvestigator)
        $ Objective
        $ forced AnyWindow
    ]

{- | The cards under the scenario reference card. Both the Quarantine step here
and resolution 1 read them; they are placed face down at setup and by agenda 3b,
so the client only ever shows their backs.
-}
getHiddenEvidence :: HasGame m => m [Card]
getHiddenEvidence = select $ UnderScenarioReferenceMatch $ CardWithType StoryType

instance RunMessage SaveOurSouls where
  runMessage msg a@(SaveOurSouls attrs) = runQueueT $ case msg of
    {- The objective resolves straight to R1; it does /not/ advance. Act 2b is
    reachable only from agenda 3b ("If you advanced this act due to agenda 3b (it
    is the only way to get here)"), so the two paths must not share a handler. -}
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure a
    {- Act 2b, "Quarantine": "Look at the story cards that are under the scenario
    reference card but do not read them. For each of the story cards, reveal 1
    random chaos token from the chaos bag. If it is not a [elder sign], [bless],
    '+1', or '0' token, the [[Crew]] story asset corresponding to that story card
    is an imitation of the Entity!" -}
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      getHiddenEvidence >>= \case
        -- Nothing hidden means nothing to suspect; the Entity still reveals
        -- itself and the crew already lost to it are still attached.
        [] -> quarantine attrs []
        hidden -> do
          lead <- getLead
          requestChaosTokens lead attrs (length hidden)
      pure a
    -- One token per hidden story card; the pairing is by position, exactly as
    -- resolution 1 does it in the scenario module.
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      continue_ iid
      hidden <- getHiddenEvidence
      quarantine attrs
        $ [ crew
          | (card, token) <- zip hidden tokens
          , isImitationToken token.face
          , Just crew <- [crewForEvidence card]
          ]
      pure a
    _ -> SaveOurSouls <$> liftRunMessage msg attrs

{- | The second half of act 2b, once the imitations are known.

"If any of the imitations are in the victory display, proceed to (->R2).
Otherwise, perform the following:
  * Spawn the set aside The Entity enemy at the Ship Mainframe.
  * Search for all [[Crew]] story assets removed from the game and attach them
    facedown to the Entity.
  * Search for all [[Crew]] story assets that are imitations of the Entity and
    attach them facedown to the Entity."
-}
quarantine :: ReverseQueue m => ActAttrs -> [CardDef] -> m ()
quarantine attrs imitations = do
  anyImitationRescued <-
    if null imitations
      then pure False
      else selectAny $ VictoryDisplayCardMatch $ basic $ mapOneOf cardIs imitations
  if anyImitationRescued
    then push R2
    else do
      -- The id is allocated up front, so the attachments below can be queued
      -- behind the creation message and still name the enemy.
      eid <- createSetAsideEnemy Enemies.theEntity Locations.shipMainframe

      {- An imitation that was scanned up is a real asset in play, so it moves as
      an entity — the same shape agenda 4a uses for crew defeated later on. One
      that never came up is still a card in the scanning deck, and one that was
      removed from the game is only a card too; both of those can only be placed
      underneath the enemy. 'getCrewAttachedToTheEntity' counts all three. -}
      imitationAssets <- select $ AssetWithTrait Crew <> mapOneOf assetIs imitations
      for_ imitationAssets \aid -> push $ PlaceAsset aid (AttachedToEnemy eid)

      scanning <- getScanningDeck
      let (imitationCards, rest) = partition ((`elem` imitations) . toCardDef) scanning
      unless (null imitationCards) $ setScenarioDeck ScanningDeck rest

      {- "attach them facedown": marked face down so the client renders card
      backs, exactly as the scenario setup does for the cards it hides under the
      reference card. The removed cards stay listed as removed from the game —
      nothing takes them back out of that area — which is why resolution 3 nubs
      the three zones it counts. -}
      removed <- getRemovedCrew
      facedown <- traverse (setFacedown True) (removed <> imitationCards)
      unless (null facedown) $ placeUnderneath eid facedown

      advanceActDeck attrs
