{- | The three Audience Participation printings (v.I, v.II, v.III) differ only in
what the ☾-release reaction costs, so their ability shapes and the whole of act
2b ("Release the Beast") live here.
-}
module Arkham.Homebrew.CircusExMortis.Acts.AudienceParticipation (
  audienceParticipationAbilities,
  audienceParticipationSealReleased,
  audienceParticipationSeal,
  audienceParticipationAdvance,
) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.ChaosToken (ChaosToken)
import Arkham.Classes.HasQueue (HasQueue)
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.FlavorText (scope, unscoped)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window
import Control.Monad.Trans.Class (MonadTrans)

blakeIsInPlay :: EnemyMatcher
blakeIsInPlay = enemyIs Enemies.sylvesterBlake

{- | "After a ☾ token is released at The Big Top": the window names the card the
token was sealed on, so the matcher reads "sealed on a card at The Big Top"
rather than "the releasing investigator is standing there". It has to see every
card type: Blake seals the ☾ tokens that let him be damaged at all, and De Cultus
Bestiae seals them on an asset.
-}
audienceParticipationAbilities :: Cost -> ActAttrs -> [Ability]
audienceParticipationAbilities releaseCost a =
  [ reaction a 1 (exists blakeIsInPlay) releaseCost
      $ ChaosTokenReleasedFrom #after (TargetAtLocation bigTopRings) moonToken
  , restricted a 2 (youExist (at_ bigTopRings) <> exists moonToken <> exists blakeIsInPlay)
      $ FastAbility (clueCost 2)
  , mkAbility a 3
      $ Objective
      $ forced
      $ EnemyWouldBeDefeated #when blakeIsInPlay
  ]

releasedToken :: [Window] -> Maybe ChaosToken
releasedToken =
  listToMaybe
    . foldMap \case
      (windowType -> Window.ChaosTokenReleased _ token) -> [token]
      _ -> []

-- | "Seal that token on Sylvester Blake."
audienceParticipationSealReleased :: ReverseQueue m => InvestigatorId -> [Window] -> m ()
audienceParticipationSealReleased iid ws = for_ (releasedToken ws) \token ->
  -- A leave-play release opens this window too, so Blake may already be gone.
  selectOne blakeIsInPlay >>= traverse_ \blake -> sealChaosToken iid blake token

-- | "Search the token bag for a ☾ token and seal it on Sylvester Blake."
audienceParticipationSeal :: ReverseQueue m => InvestigatorId -> m ()
audienceParticipationSeal iid =
  selectOne blakeIsInPlay >>= traverse_ (sealMoonTokenOnTarget iid)

{- | Act 2b. Blake is never actually defeated: the "would be defeated" objective
advances instead, so the queued defeat is cancelled and he flips into The Black
Goat (the @Flip@ handler on the enemy does the 'ReplaceEnemy').
-}
audienceParticipationAdvance
  :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m)) => ActAttrs -> t m ()
audienceParticipationAdvance attrs = do
  blake <- selectJust blakeIsInPlay
  cancelEnemyDefeat blake
  healAllDamage attrs blake
  lead <- getLead
  flipOverBy lead attrs blake
  -- Swap keeps the enemy id, so the flipped Black Goat is still @blake@.
  monstrousTransformation attrs blake
  advanceActDeck attrs

{- | "If an investigator has Monstrous Transformation in their deck": the Curse of
the Rougarou side story hands the asset out as a campaign story card, so its
owner is the investigator who resolves this.
-}
monstrousTransformation :: ReverseQueue m => ActAttrs -> EnemyId -> m ()
monstrousTransformation attrs blackGoat =
  getOwner Assets.monstrousTransformation >>= traverse_ \iid -> do
    inPlay <- selectAny $ assetIs Assets.monstrousTransformation
    moonInBag <- selectAny moonToken
    campaignI18n $ scope "audienceParticipation" $ chooseOneM iid do
      if inPlay
        then labeledValidate moonInBag "sealOnTheBlackGoat" $ sealMoonTokenOnTarget iid blackGoat
        else
          labeled "putMonstrousTransformationIntoPlay"
            $ search
              iid
              attrs
              iid
              [fromDeck, (FromHand, PutBack), fromDiscard]
              (basic $ cardIs Assets.monstrousTransformation)
              (PlayFoundNoCost iid 1)
      unscoped $ labeled "doNothing" nothing
