module Arkham.Event.Cards.LetMeHandleThis (
  letMeHandleThis,
  letMeHandleThisEffect,
  LetMeHandleThis (..),
) where

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Prelude

newtype LetMeHandleThis = LetMeHandleThis EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letMeHandleThis :: EventCard LetMeHandleThis
letMeHandleThis = event LetMeHandleThis Cards.letMeHandleThis

instance RunMessage LetMeHandleThis where
  runMessage msg e@(LetMeHandleThis attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ (cardDrawn -> card) _ | eid == eventId -> do
      let cardId = toCardId card

      mTreachery <- selectOne $ TreacheryWithCardId cardId
      mEnemy <- selectOne $ EnemyWithCardId cardId
      mLocation <- selectOne $ LocationWithCardId cardId
      mAsset <- selectOne $ AssetWithCardId cardId
      mEvent <- selectOne $ EventWithCardId cardId

      let
        baseReplace :: Sourceable source => source -> Message -> Message
        baseReplace (toSource -> source) = \case
          Revelation _ source' | source == source' -> Revelation iid source
          ResolvedCard _ encounterCard | toCardId encounterCard == cardId -> ResolvedCard iid encounterCard
          Surge _ source' | source == source' -> Surge iid source
          other -> other

      for_ mTreachery \tid -> do
        withQueue_ $ map $ \case
          AfterRevelation _ tid' | tid == tid' -> AfterRevelation iid tid'
          other -> baseReplace tid other

      for_ mEnemy \enemyId -> do
        withQueue_ $ map $ \case
          InvestigatorDrawEnemy _ enemyId' | enemyId == enemyId' -> InvestigatorDrawEnemy iid enemyId'
          other -> baseReplace enemyId other

      for_ mLocation \tid -> withQueue_ $ map (baseReplace tid)
      for_ mAsset \aid -> withQueue_ $ map (baseReplace aid)
      for_ mEvent \aid -> withQueue_ $ map (baseReplace aid)

      when (isNothing $ (mTreachery $> ()) <|> (mEnemy $> ()) <|> (mLocation $> ()) <|> (mAsset $> ()))
        $ error "Unhandled card type"

      pushAll [createCardEffect Cards.letMeHandleThis Nothing attrs iid]
      pure e
    _ -> LetMeHandleThis <$> runMessage msg attrs

newtype LetMeHandleThisEffect = LetMeHandleThisEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letMeHandleThisEffect :: EffectArgs -> LetMeHandleThisEffect
letMeHandleThisEffect = cardEffect LetMeHandleThisEffect Cards.letMeHandleThis

instance HasModifiersFor LetMeHandleThisEffect where
  getModifiersFor target (LetMeHandleThisEffect a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a $ AnySkillValue 2]
  getModifiersFor _ _ = pure []

instance RunMessage LetMeHandleThisEffect where
  runMessage msg e@(LetMeHandleThisEffect attrs) = case msg of
    AfterRevelation _ tid' | attrs.target == TreacheryTarget tid' -> do
      push $ disable attrs
      pure e
    _ -> LetMeHandleThisEffect <$> runMessage msg attrs
