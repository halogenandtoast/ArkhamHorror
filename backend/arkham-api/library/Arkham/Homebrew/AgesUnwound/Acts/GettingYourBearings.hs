module Arkham.Homebrew.AgesUnwound.Acts.GettingYourBearings (gettingYourBearings) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern ArkhamStreetsDeck)
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Arkham))

newtype GettingYourBearings = GettingYourBearings ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gettingYourBearings :: ActCard GettingYourBearings
gettingYourBearings = act (2, A) GettingYourBearings Cards.gettingYourBearings Nothing

{- | The permission /On the Lam/ spells out in parentheses. This act does not print it,
but its reaction is the same one and act 2 can reach the same dead end -- a freshly
dealt street need not connect to anything -- so without it the scenario could stall
with no legal move at all. See 'redirectingAct'.
-}
instance HasModifiersFor GettingYourBearings where
  getModifiersFor (GettingYourBearings a) =
    whenM (notNull <$> getArkhamStreetsDeck)
      $ modifySelect a Anywhere [ConnectsToSelfWhenIsolated]

-- NB. index 5 was a stand-in move action for the permission now granted above; left
-- unused so a game parked on a question that offered it cannot resolve something else.
instance HasAbilities GettingYourBearings where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ triggered_ redirectableMove
    , restricted a 2 (exists $ #empty <> LocationWithResources (atMost 0)) $ forced $ RoundEnds #when
    , restricted a 3 (exists $ You <> at_ Anywhere)
        $ freeTrigger (GroupClueCost (PerPlayer 1) YourLocation)
    , onlyOnce
        $ restricted a 4 (LocationCount 8 (withTrait Arkham) <> notExists (LocationWithResources $ atMost 0))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage GettingYourBearings where
  runMessage msg a@(GettingYourBearings attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      redirectToNewArkhamStreetsLocation (attrs.ability 1) iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      cards <- selectForM (#empty <> LocationWithResources (atMost 0)) \lid -> do
        removeLocationWithoutVictory lid
        fetchCard lid
      shuffleCardsIntoDeck ArkhamStreetsDeck cards
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      withMatch (locationWithInvestigator iid) $ placeTokensOn (attrs.ability 3) #resource 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 4 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      n <- perPlayer 1
      selectEach
        ( mapOneOf
            locationIs
            [Locations.aPlaceToHide, Locations.winchmoreHouse, Locations.crampedPassage]
        )
        $ placeTokensOn attrs #clue n

      lead <- getLead
      findEncounterCard lead attrs (cardIs Enemies.hiredThugs)

      selectEach
        (mapOneOf enemyIs [Enemies.eternitysSentinel_016, Enemies.eternitysSentinel_017])
        \sentinel -> do
          isScourge <- sentinel <=~> enemyIs Enemies.eternitysSentinel_016
          when isScourge do
            watcher <- getSetAsideCard Enemies.eternitysSentinel_017
            push $ ReplaceEnemy sentinel watcher Swap
          placement <- field EnemyPlacement sentinel
          when (placement == InTheShadows)
            $ push
            $ EnemySpawnAtLocationMatching Nothing (locationIs Locations.rivertown) sentinel

      advanceActDeck attrs
      pure a
    FoundEncounterCard _ (isTarget attrs -> True) (toCard -> card) -> do
      selectForMaybeM (locationIs Locations.winchmoreHouse) \winchmoreHouse -> do
        eid <- createEnemy card winchmoreHouse
        n <- perPlayer 1
        gameModifiers attrs eid [HealthModifier n, RemoveKeyword Keyword.Hunter]
      pure a
    _ -> GettingYourBearings <$> liftRunMessage msg attrs
