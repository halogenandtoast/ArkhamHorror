module Arkham.Homebrew.CircusExMortis.Assets.DarkOfTheMoon (darkOfTheMoon) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers (
  flipToVictoryDisplay,
  investigatorWithDestiny,
  investigatorWithDestinyModifier,
  moonToken,
 )
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

{- | The back of the Bear the Burden Destiny story (:208b), in play next to the act deck.

"The investigator whose destiny is \"burden\" may activate abilities on this card at any
location." The card is at no location, so that seat is the entire criterion on every
ability and no location requirement is imposed -- which is what "at any location" means.
-}
newtype DarkOfTheMoon = DarkOfTheMoon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkOfTheMoon :: AssetCard DarkOfTheMoon
darkOfTheMoon = asset DarkOfTheMoon Cards.darkOfTheMoon

{- | "5 or more ☾ tokens sealed among cards you control". Counted as two calculations
rather than one 'ChaosTokenMatchesAny', because only a top-level @SealedOn*@ puts sealed
tokens in the candidate pool. The two pools that can hold a ☾ in this scenario are the
investigator's own card (where everything that seals here puts them) and an asset they
control (De Cultus Bestiae).
-}
sealedMoonTokensYouControl :: GameCalculation
sealedMoonTokensYouControl =
  SumCalculation
    [ CountChaosTokens $ SealedOnInvestigator You moonToken
    , CountChaosTokens $ SealedOnAsset (AssetControlledBy You) moonToken
    ]

{- | 1. "[action] If there are 5 or more ☾ tokens sealed among cards you control: Flip Dark
of the Moon and add it to the victory display." The condition is the whole effect, so it is
a criterion: below 5 the action is not offered at all.
2. "[action] [action]: Reveal 4 random chaos tokens from the bag..."
-}
instance HasAbilities DarkOfTheMoon where
  getAbilities (DarkOfTheMoon a) =
    [ restricted a 1 (burdenSeat <> HasCalculation sealedMoonTokensYouControl (atLeast 5)) actionAbility
    , restricted a 2 burdenSeat $ ActionAbility mempty Nothing (ActionCost 2)
    ]
   where
    burdenSeat = youExist (investigatorWithDestinyModifier "burden")

{- | "Seal each ☾ token revealed on your investigator card." Sealing takes the token out of
the chaos bag's set-aside pile as well, so the reveal's own reset cannot hand it back.
-}
sealRevealedMoonTokens :: ReverseQueue m => InvestigatorId -> [ChaosToken] -> m ()
sealRevealedMoonTokens iid tokens =
  for_ (filter ((== MoonToken) . (.face)) tokens) $ sealChaosToken iid iid

instance RunMessage DarkOfTheMoon where
  runMessage msg a@(DarkOfTheMoon attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      flipToVictoryDisplay Nothing Stories.bearTheBurden attrs.cardId (RemoveAsset attrs.id)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      requestChaosTokens iid (attrs.ability 2) 4
      pure a
    {- The printed four. "Investigators at your location may place any amount of clues they
    control on that location to reveal 2 additional tokens per clue placed" is resolved
    after them, in printed order, so the table decides how many more to pay for knowing
    what the four were. -}
    RequestedChaosTokens (isAbilitySource attrs 2 -> True) (Just iid) tokens -> do
      sealRevealedMoonTokens iid tokens
      -- The four are already on screen, so the questions below are what hold them there.
      withLocationOf iid \lid -> do
        payers <- select $ InvestigatorAt (LocationWithId lid) <> InvestigatorWithAnyClues
        if null payers
          then chooseOneM iid $ withI18n $ labeled "continue" nothing
          else for_ payers \i -> do
            clues <- field InvestigatorClues i
            withI18n $ chooseAmount i "clues" "$clues" 0 clues attrs
      pure a
    ResolveAmounts placer (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      push $ InvestigatorPlaceCluesOnLocation placer (attrs.ability 2) n
      {- "Seal each ☾ token revealed on your investigator card": "your" is the seat that
      activated the ability, not whoever paid the clues, and only the "burden" seat can
      activate this card at all -- so that is who the extra reveal is resolved for. The
      extra draw carries an 'IndexedSource' so it lands in its own arm below instead of
      re-opening the clue question. -}
      burden <- selectOne =<< investigatorWithDestiny "burden"
      for_ burden \iid -> requestChaosTokens iid (IndexedSource n (attrs.ability 2)) (2 * n)
      pure a
    RequestedChaosTokens (IndexedSource _ (isAbilitySource attrs 2 -> True)) (Just iid) tokens -> do
      sealRevealedMoonTokens iid tokens
      chooseOneM iid $ withI18n $ labeled "continue" nothing
      pure a
    _ -> DarkOfTheMoon <$> liftRunMessage msg attrs
