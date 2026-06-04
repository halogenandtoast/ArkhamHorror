module Arkham.Act.Cards.LostSelf (lostSelf) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Helpers (unDeck)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Helpers.Query (getInvestigators, getSetAsideCardMaybe, getSetAsideCardsMatching)
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Layout
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.FateOfTheVale.Helpers
import Arkham.SkillTest
import Arkham.SkillTestResult
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Cave, Emissary, Lair))
import Data.List (cycle)

newtype LostSelf = LostSelf ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostSelf :: ActCard LostSelf
lostSelf = actWith (2, A) LostSelf Cards.lostSelf Nothing (metaL .~ toJSON True)

instance HasAbilities LostSelf where
  getAbilities (LostSelf a) =
    extend
      a
      $ [ restricted a 1 (DuringTurn You)
            $ freeReaction
            $ SkillTestResult #after You (SkillTestAt $ LocationWithTrait Lair) #success
        | not (toResultDefault False a.meta)
        ]
      <> [ restricted a 2 DuringYourSkillTest $ FastAbility $ ClueCost $ Static 1
         , onlyOnce
             $ restricted
               a
               3
               (LocationCount 3 $ RevealedLocation <> LocationWithTrait Cave <> LocationWithoutClues)
             $ Objective
             $ forced
             $ RoundEnds #when
         ]

-- | The asset-side card def of each resident, used to find set-aside residents.
residentAssetDefs :: [CardDef]
residentAssetDefs = map toCardDef [minBound .. maxBound :: Resident]

hemlockValeLayout :: [GridTemplateRow]
hemlockValeLayout =
  [ ".     triangle square"
  , "moon  triangle square"
  , "moon  diamond  star"
  , "heart diamond  star"
  , "heart circle   spade"
  , ".     circle   spade"
  ]

placeSetAsideNightLocation :: ReverseQueue m => CardDef -> m LocationId
placeSetAsideNightLocation def = do
  mcard <- getSetAsideCardMaybe def
  case mcard of
    Just card -> do
      -- getSetAsideCardMaybe can return the requested side with the original
      -- card id, but the global card cache may still know that id as the Day
      -- side. Replace the cached card so skill test windows show the Night
      -- side when these locations are the source of tests.
      push $ ReplaceCard (toCardId card) card
      placeLocation card
    Nothing -> placeLocation def

{- | Shared "Hemlock Vale (Night)" setup (Fate of the Vale 2 and 5): remove the
Mirror Nest and Cave locations along with the Cosmic Emissary enemies, put each
set-aside Hemlock Vale location into play on its (Night) side, and gather the
investigators at The Crossroads. The new village locations are only in play on
the following step, so callers that need to query them should do so from a
subsequent @DoStep@.
-}
setupNightVale :: ReverseQueue m => ActAttrs -> m ()
setupNightVale attrs = do
  selectEach (EnemyWithTrait Emissary) removeFromGame
  push $ SetLayout hemlockValeLayout
  crossroads <- placeSetAsideNightLocation Locations.theCrossroadsNight
  traverse_
    placeSetAsideNightLocation
    [ Locations.boardingHouseNight
    , Locations.hemlockChapelNight
    , Locations.theOldMillNight
    , Locations.theAtwoodHouseNight
    , Locations.tadsGeneralStoreNight
    , Locations.valeSchoolhouseNight
    , Locations.theCommonsNight
    ]
  eachInvestigator \iid -> moveTo attrs iid crossroads
  selectEach (LocationWithTitle "Mirror Nest") removeLocation
  selectEach (LocationWithTrait Cave) removeLocation

{- | Flip each Cosmic Emissary to its Shattered side and place them across the
village: Cosmic Emissary (The Abyss) at The Crossroads, and the other three,
shuffled, at The Boarding House, The Commons, and The Atwood House.
-}
flipEmissariesAtVillage :: ReverseQueue m => m ()
flipEmissariesAtVillage = do
  let createBrokenFormationEmissary emissary lid = do
        eid <- createEnemyAt emissary lid
        scenarioSpecific "disableAsSelfLocation" eid

  crossroads <- selectJust $ locationIs Locations.theCrossroadsNight
  createBrokenFormationEmissary Enemies.cosmicEmissaryTheAbyssShattered crossroads
  others <-
    shuffle
      [ Enemies.cosmicEmissaryTheMiasmaShattered
      , Enemies.cosmicEmissaryTheBrillianceShattered
      , Enemies.cosmicEmissaryThePhantasmShattered
      ]
  houses <-
    traverse
      (selectJust . locationIs)
      [Locations.boardingHouseNight, Locations.theCommonsNight, Locations.theAtwoodHouseNight]
  for_ (zip others houses) (uncurry createBrokenFormationEmissary)

placeEnemySideAt :: ReverseQueue m => Card -> LocationId -> m ()
placeEnemySideAt card lid = do
  obtainCard card
  case residentFromCardDef (toCardDef card) of
    Just resident -> createEnemyAt_ (residentEnemyDef resident) lid
    Nothing -> createEnemyAt_ card lid

chooseEnemyPlacements :: ReverseQueue m => InvestigatorId -> [Card] -> [LocationId] -> m ()
chooseEnemyPlacements _ [] _ = pure ()
chooseEnemyPlacements _ _ [] = pure ()
chooseEnemyPlacements lead (card : cards) locs =
  focusCards [card] do
    chooseTargetM lead locs \lid -> do
      unfocusCards
      placeEnemySideAt card lid
      chooseEnemyPlacements lead cards (filter (/= lid) locs)

{- | Search the Day of the Feast set for 2 Frenzied Revelers, shuffle them with
each set-aside Resident card, and place them, enemy side faceup, beneath each
location except The Crossroads, distributed as evenly as possible.

For Fate of the Vale 5, cards are placed at empty locations. Stop once there are
no empty locations remaining; if there are too few cards for every empty
location, the lead investigator chooses which empty locations receive them.
-}
seedResidentsBeneath :: ReverseQueue m => Bool -> m ()
seedResidentsBeneath onlyEmpty = do
  crossroads <- selectJust $ locationIs Locations.theCrossroadsNight
  locs <-
    select
      $ (if onlyEmpty then EmptyLocation else Anywhere)
      <> not_ (be crossroads)
  revelers <- take 2 <$> getSetAsideCardsMatching (cardIs Enemies.frenziedReveler)
  residents <- getSetAsideCardsMatching (mapOneOf cardIs residentAssetDefs)
  cards <- shuffle (revelers <> residents)
  unless (null locs || null cards) do
    if onlyEmpty
      then
        if length cards >= length locs
          then for_ (zip cards locs) (uncurry placeEnemySideAt)
          else getLead >>= \lead -> chooseEnemyPlacements lead cards locs
      else for_ (zip cards (cycle locs)) \(card, lid) -> placeUnderneath lid [card]

{- | Put a story ally into play under the lead investigator's control from the
set-aside pile, unless it is already in play.
-}
putAllyIntoPlay :: ReverseQueue m => InvestigatorId -> CardDef -> m ()
putAllyIntoPlay iid def = do
  inPlay <- selectAny (assetIs def)
  unless inPlay $ getSetAsideCardMaybe def >>= traverse_ (takeControlOfSetAsideAsset iid)

abyssRevealedLayout :: [GridTemplateRow]
abyssRevealedLayout =
  [ ". . caveTop1 caveTop2 . ."
  , ". . . mirrorNestTop . ."
  , "caveLeft1 mirrorNestLeft theAbyss theAbyss . ."
  , "caveLeft2 . theAbyss theAbyss mirrorNestRight caveRight1"
  , ". . mirrorNestBottom . . caveRight2"
  , ". . caveBottom1 caveBottom2 . ."
  ]

flipTheAbyssStoryToLocation :: ReverseQueue m => m LocationId
flipTheAbyssStoryToLocation = do
  push $ SetLayout abyssRevealedLayout
  existing <- selectOne $ locationIs Locations.theAbyssSpiralingOblivion
  case existing of
    Just lid -> do
      setLocationLabel lid "theAbyss"
      unsafeReveal lid
      scenarioSpecific "theAbyssBecameLocation" Null
      pure lid
    Nothing -> do
      -- The Abyss deck remains the scenario deck keyed by AbyssDeck; do not move
      -- those cards beneath the location object or the scenario-deck operations
      -- that reveal from the bottom/top of The Abyss will stop working.
      getSetAsideCardMaybe Stories.theAbyss >>= traverse_ obtainCard
      abyssDeck <- getScenarioDeck AbyssDeck
      card <- case find ((== Locations.theAbyssSpiralingOblivion) . toCardDef) abyssDeck of
        Just c -> do
          scenarioSpecific "removeFromAbyss" (toCardId c)
          pure c
        Nothing -> fromMaybeM (genCard Locations.theAbyssSpiralingOblivion) do
          getSetAsideCardMaybe Locations.theAbyssSpiralingOblivion
      lid <- placeLocation card
      setLocationLabel lid "theAbyss"
      unsafeReveal lid
      selectEach (LocationWithTitle "Mirror Nest") $ connectBothWays lid
      scenarioSpecific "theAbyssBecameLocation" Null
      pure lid

flipCosmicEmissariesToShatteredFormation :: ReverseQueue m => LocationId -> m ()
flipCosmicEmissariesToShatteredFormation abyss = do
  -- In the app representation, each non-Abyss Emissary is at the Mirror Nest it
  -- physically borders. Preserve that location while replacing it with its
  -- Shattered side; the Abyss Emissary moves to the newly revealed Abyss.
  for_
    [ (Enemies.cosmicEmissaryTheMiasma, Enemies.cosmicEmissaryTheMiasmaShattered)
    , (Enemies.cosmicEmissaryTheBrilliance, Enemies.cosmicEmissaryTheBrillianceShattered)
    , (Enemies.cosmicEmissaryThePhantasm, Enemies.cosmicEmissaryThePhantasmShattered)
    ]
    \(normal, shattered) ->
      selectEach (enemyIs normal) \eid -> do
        mloc <- field EnemyLocation eid
        removeFromGame eid
        for_ mloc \lid -> do
          shatteredEid <- createEnemyAt shattered lid
          scenarioSpecific "disableAsSelfLocation" shatteredEid
  selectEach (enemyIs Enemies.cosmicEmissaryTheAbyss) removeFromGame
  shatteredAbyss <- createEnemyAt Enemies.cosmicEmissaryTheAbyssShattered abyss
  scenarioSpecific "disableAsSelfLocation" shatteredAbyss

revealFromBottomOfAbyss :: ReverseQueue m => InvestigatorId -> Int -> m ()
revealFromBottomOfAbyss iid n = do
  abyss <- getScenarioDeck AbyssDeck
  let revealed = drop (max 0 (length abyss - n)) abyss
  unless (null revealed) $ focusCards revealed do
    chooseOneM iid do
      targets revealed \card -> do
        unfocusCards
        let rest = filter (/= card) revealed
        for_ revealed $ scenarioSpecific "removeFromAbyss" . toCardId
        shuffleCardsIntoTopOfDeck (Deck.ScenarioDeckByKey AbyssDeck) 0 rest
        scenarioSpecific "drawFromAbyss" (iid, card)

instance RunMessage LostSelf where
  runMessage msg a@(LostSelf attrs) = runQueueT $ case msg of
    EndSkillTestWindow -> pure $ LostSelf $ attrs & metaL .~ toJSON False
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getSkillTest >>= traverse_ \st -> case skillTestResult st of
        SucceededBy _ n | n > 0 -> revealFromBottomOfAbyss iid n
        _ -> pure ()
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (AnySkillValue 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceVia #other attrs (attrs.ability 3)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> scenarioI18n $ scope "interlude" do
      eachInvestigator $ discardAllClues attrs
      lead <- getLead
      canEndThis <-
        (||)
          <$> ((&&) <$> getHasRecord DrMarquesHasAHunch <*> getHasRecord DrMarquezHasAPlan)
          <*> getHasRecord GideonFinishedTheTaleOfAnnabelleLee
      canSaveTheVale <-
        (||) <$> getHasRecord TheHemlocksMadeATruce <*> getHasRecord ThePetersFamilyWereReunited
      canBurnItAll <- getHasRecord TheValeIsFullOfFireworks
      chooseOneM lead do
        when canEndThis $ labeled' "letsEndThis" $ doStep 1 msg
        when canSaveTheVale $ labeled' "saveTheVale" $ doStep 2 msg
        when canBurnItAll $ labeled' "burnItAll" $ doStep 3 msg
        labeled' "escape" $ doStep 4 msg
      pure a
    -- Fate of the Vale 1: "Let's end this."
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      storyWithContinue' $ setTitle "title" >> p "fateOfTheVale1"

      -- If Dr. Marquez is not already in play, search each investigator's deck
      -- and all out-of-play areas for her and put her into play.
      marquezInPlay <- selectAny (assetIs Assets.drRosaMarquezBestInHerField)
      unless marquezInPlay do
        investigators <- getInvestigators
        owners <-
          filterM
            ( fieldMap
                InvestigatorDeck
                (any ((== Assets.drRosaMarquezBestInHerField) . toCardDef) . unDeck)
            )
            investigators
        case owners of
          (iid : _) -> putCampaignCardIntoPlay iid Assets.drRosaMarquezBestInHerField
          [] -> do
            lead <- getLead
            getSetAsideCardMaybe Assets.drRosaMarquezBestInHerField
              >>= traverse_ (takeControlOfSetAsideAsset lead)

      abyss <- flipTheAbyssStoryToLocation

      flipCosmicEmissariesToShatteredFormation abyss

      advanceToAct attrs Cards.fateOfTheValeV1 A
      pure a
    -- Fate of the Vale 2: "Save the Vale!"
    DoStep 2 adv@(AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      storyWithContinue' $ setTitle "title" >> p "fateOfTheVale2"
      setupNightVale attrs

      lead <- getLead
      whenHasRecord ThePetersFamilyWereReunited do
        putAllyIntoPlay lead Assets.helenPetersTheEldestSister
        putAllyIntoPlay lead Assets.theoPetersJackOfAllTrades
      whenHasRecord TheHemlocksMadeATruce do
        putAllyIntoPlay lead Assets.riverHawthorneBigInNewYork
        putAllyIntoPlay lead Assets.williamHemlockAspiringPoet

      -- The continuation must run before the act is replaced (which removes this
      -- act from play), so push it ahead of advanceToAct.
      doStep 102 adv
      advanceToAct attrs Cards.fateOfTheValeV2 A
      pure a
    DoStep 102 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      seedResidentsBeneath False
      flipEmissariesAtVillage
      pure a
    -- Fate of the Vale 3: "Burn it all."
    DoStep 3 adv@(AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      storyWithContinue' $ setTitle "title" >> p "fateOfTheVale3"
      -- Skip to Fate of the Vale 5; advance the act after that shared step has
      -- been queued so this act is still in play to handle it.
      doStep 5 adv
      advanceToAct attrs Cards.fateOfTheValeV3 A
      pure a
    -- Fate of the Vale 4: "Escape with our lives."
    DoStep 4 adv@(AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      storyWithContinue' $ setTitle "title" >> p "fateOfTheVale4"
      doStep 5 adv
      advanceToAct attrs Cards.fateOfTheValeV4 A
      pure a
    -- Fate of the Vale 5: shared resolution for v.III and v.IV.
    DoStep 5 adv@(AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      storyWithContinue' $ setTitle "title" >> p "fateOfTheVale5"
      setupNightVale attrs
      doStep 105 adv
      pure a
    DoStep 105 adv@(AdvanceAct (isSide B attrs -> True) _ _) -> do
      bertiePerished <- getHasRecord BertiePerished
      if bertiePerished
        then
          getSetAsideCardMaybe Assets.bertieMusgraveATrueAesthete >>= traverse_ (removeFromGame . toCardId)
        else do
          boardingHouse <- selectJust $ locationIs Locations.boardingHouseNight
          getSetAsideCardMaybe Assets.bertieMusgraveATrueAesthete
            >>= traverse_ \c -> void $ createAssetAt c (AtLocation boardingHouse)
      seedResidentsBeneath True
      flipEmissariesAtVillage
      doStep 106 adv
      pure a
    DoStep 106 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      -- Remove each remaining (unplaced) Resident card from the game.
      remaining <- getSetAsideCardsMatching (mapOneOf cardIs residentAssetDefs)
      for_ remaining (removeFromGame . toCardId)
      pure a
    _ -> LostSelf <$> liftRunMessage msg attrs
