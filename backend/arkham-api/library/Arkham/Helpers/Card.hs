module Arkham.Helpers.Card (
  module Arkham.Helpers.Card,
  module Arkham.Helpers.Campaign,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ActiveCost.Base
import Arkham.Asset.Types
import Arkham.Card
import Arkham.ChaosBag.Base (chaosBagChaosTokens)
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Deck
import Arkham.Enemy.Types
import {-# SOURCE #-} Arkham.Entities
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign
import Arkham.Helpers.Matchers
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Id
import Arkham.Keyword (Sealing (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Types
import Arkham.Matcher hiding (AssetCard, LocationCard)
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Types
import Data.Proxy

isDiscardable :: Card -> Bool
isDiscardable = not . isWeakness
 where
  isWeakness = \case
    PlayerCard pc -> isJust $ cdCardSubType $ toCardDef pc
    EncounterCard _ -> True -- maybe?
    VengeanceCard _ -> False -- should be an error

getCardPayments :: HasGame m => Card -> m (Maybe Payment)
getCardPayments c = do
  costs <- getActiveCosts
  pure $ activeCostPayments <$> find (isCardTarget . activeCostTarget) costs
 where
  isCardTarget = \case
    ForAbility {} -> False
    ForAdditionalCost {} -> False
    ForCard _ c' -> toCardId c == toCardId c'
    ForCost c' -> toCardId c == toCardId c'

extendedCardMatch
  :: (HasGame m, IsCard c) => c -> ExtendedCardMatcher -> m Bool
extendedCardMatch (toCard -> c) matcher =
  selectAny (basic (CardWithId c.id) <> matcher)

class ConvertToCard a where
  convertToCard :: (HasCallStack, HasGame m) => a -> m Card

instance ConvertToCard TreacheryId where
  convertToCard = getEntityCard @Treachery

instance ConvertToCard EnemyId where
  convertToCard eid =
    fmap (fromJustNote ("Could not convert entity: " <> show eid) . asum)
      $ sequence
      $ getEntityCardMaybe @Enemy eid
      : overOutOfPlayZones (\(_ :: Proxy zone) -> getEntityCardMaybe @(OutOfPlayEntity zone Enemy) eid)

instance ConvertToCard AssetId where
  convertToCard = getEntityCard @Asset

instance ConvertToCard LocationId where
  convertToCard = getEntityCard @Location

instance ConvertToCard Card where
  convertToCard = pure

instance ConvertToCard CardId where
  convertToCard = getCard

class (Projection a, Entity a) => CardEntity a where
  cardField :: Field a Card

getEntityCard :: forall a m. (HasCallStack, CardEntity a, HasGame m) => EntityId a -> m Card
getEntityCard = field (cardField @a)

getEntityCardMaybe :: forall a m. (CardEntity a, HasGame m) => EntityId a -> m (Maybe Card)
getEntityCardMaybe = fieldMay (cardField @a)

instance CardEntity Treachery where
  cardField = TreacheryCard

instance CardEntity Enemy where
  cardField = EnemyCard

instance KnownOutOfPlayZone zone => CardEntity (OutOfPlayEntity zone Enemy) where
  cardField = OutOfPlayEnemyField (knownOutOfPlayZone (Proxy @zone)) EnemyCard

instance CardEntity Asset where
  cardField = AssetCard

instance CardEntity Location where
  cardField = LocationCard

getCardField :: (HasCallStack, ConvertToCard c, HasGame m) => (CardDef -> a) -> c -> m a
getCardField f c = f . toCardDef <$> convertToCard c

getVictoryPoints :: (ConvertToCard c, HasGame m) => c -> m (Maybe Int)
getVictoryPoints c = do
  card <- convertToCard c
  printedVictory <- getPrintedVictoryPoints card
  modifiers' <- getModifiers $ toCardId card
  if LoseVictory `elem` modifiers'
    then pure Nothing
    else pure $ foldr applyModifier printedVictory modifiers'
 where
  applyModifier (GainVictory n) m = Just (n + fromMaybe 0 m)
  applyModifier _ n = n

getHasVictoryPoints :: (ConvertToCard c, HasGame m) => c -> m Bool
getHasVictoryPoints c = isJust <$> getVictoryPoints c

getPrintedVictoryPoints :: (ConvertToCard c, HasGame m) => c -> m (Maybe Int)
getPrintedVictoryPoints = getCardField cdVictoryPoints

-- To get abilities we convert to some entity in Entities and get all abilities
getCardAbilities :: InvestigatorId -> Card -> [Ability]
getCardAbilities iid c = getAbilities $ addCardEntityWith iid id mempty c

findJustCard :: HasGame m => (Card -> Bool) -> m Card
findJustCard cardPred = fromJustNote "invalid card" <$> findCard cardPred

findUniqueCard :: HasGame m => CardDef -> m Card
findUniqueCard def = findJustCard (`cardMatch` (cardIs def <> CardIsUnique))

iconsForCard :: HasGame m => Card -> m [SkillIcon]
iconsForCard c@(PlayerCard MkPlayerCard {..}) = do
  mods <- getModifiers (CardIdTarget pcId)
  let wildReplace = if ReplaceAllSkillIconsWithWild `elem` mods then const #wild else id
  pure
    $ map wildReplace
    $ foldr applyAfter (foldr apply (cdSkills $ toCardDef c) mods) mods
 where
  apply (AddSkillIcons xs) ys = xs <> ys
  apply (RemoveSkillIcons xs) ys = ys \\ xs
  apply _ ys = ys
  applyAfter DoubleSkillIcons ys = ys <> ys
  applyAfter _ ys = ys
iconsForCard _ = pure []

getCardEntityTarget :: HasGame m => Card -> m (Maybe Target)
getCardEntityTarget card = case toCardType card of
  EnemyType -> toTarget <$$> selectOne (EnemyWithCardId $ toCardId card)
  PlayerEnemyType -> toTarget <$$> selectOne (EnemyWithCardId $ toCardId card)
  TreacheryType -> toTarget <$$> selectOne (TreacheryWithCardId $ toCardId card)
  PlayerTreacheryType -> toTarget <$$> selectOne (TreacheryWithCardId $ toCardId card)
  LocationType -> toTarget <$$> selectOne (LocationWithCardId $ toCardId card)
  AssetType -> toTarget <$$> selectOne (AssetWithCardId $ toCardId card)
  EncounterAssetType -> toTarget <$$> selectOne (AssetWithCardId $ toCardId card)
  other -> error $ "Unhandled type: " <> show other

-- NOTE: this used to call drawThisPlayerCard but we changed that because we
-- weren't triggering Foresight (1). If we have to revert this, remember to
-- check foresight (1).
drawThisCardFrom :: IsCard card => InvestigatorId -> card -> Maybe DeckSignifier -> [Message]
drawThisCardFrom iid card mdeck = case toCard card of
  c@(PlayerCard pc) -> [InvestigatorDrewPlayerCardFrom iid pc mdeck, ResolvedCard iid c]
  EncounterCard _ -> error "Not yet implemented"
  VengeanceCard c -> drawThisCardFrom iid c mdeck

-- drawThisPlayerCard :: InvestigatorId -> PlayerCard -> [Message]
-- drawThisPlayerCard iid card = case toCardType card of
--   PlayerTreacheryType ->
--     [ DrewTreachery iid (Just $ Deck.InvestigatorDeck iid) (PlayerCard card)
--     , ResolvedCard iid (PlayerCard card)
--     ]
--   PlayerEnemyType -> do
--     if hasRevelation card
--       then [Revelation iid $ CardIdSource card.id, ResolvedCard iid (PlayerCard card)]
--       else [DrewPlayerEnemy iid (PlayerCard card), ResolvedCard iid (PlayerCard card)]
--   other | hasRevelation card && other `notElem` [PlayerTreacheryType, PlayerEnemyType] -> do
--     [Revelation iid (CardIdSource card.id), ResolvedCard iid (PlayerCard card)]
--   _ -> []

playIsValidAfterSeal :: HasGame m => InvestigatorId -> Card -> m Bool
playIsValidAfterSeal iid c = do
  tokens <- scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens
  let
    sealingToMatcher = \case
      Sealing matcher -> Just matcher
      SealOneOf (m1 :| rest) -> Just $ oneOf $ mapMaybe sealingToMatcher (m1 : rest)
      SealUpTo _ matcher -> Just matcher
      SealUpToX _ -> Nothing
    sealChaosTokenMatchers = flip mapMaybe (setToList c.keywords) \case
      Keyword.Seal sealing -> sealingToMatcher sealing
      _ -> Nothing
  allM (\matcher -> anyM (\t -> matchChaosToken iid t matcher) tokens) sealChaosTokenMatchers
