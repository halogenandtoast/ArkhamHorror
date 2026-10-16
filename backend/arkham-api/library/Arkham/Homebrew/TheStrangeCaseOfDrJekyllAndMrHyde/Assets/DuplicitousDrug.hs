{- | Duplicitous Drug holds the transformation, so it also holds what the
transformation displaces.

"Set your investigator card aside with all tokens on it" has no engine
equivalent: 'TransfiguredForm' only changes whose card front is read, and damage
and horror live on the outer investigator attrs, so a form change leaves them
untouched. They are taken off the investigator here and kept in this asset's
meta ('setAsideTokens') until Your Other Half's reversion puts them back -- an
investigator who transformed on 2 damage would otherwise be defeated instantly
by Your Other Half's 1 health. Resources and clues share that same token map but
stay where they are: they are yours rather than the card's, and you go on
spending them while transformed.

The drug is the stash's home because it is unique, it is the only route into the
form, and it stays in play for the whole transformation, whereas
'investigatorFormMeta' starts uninitialized every time a form is entered. The
cost: if the drug somehow leaves play while you are transformed, the stash goes
with it and you revert undamaged.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Assets.DuplicitousDrug (
  duplicitousDrug,
  setAsideTokens,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Classes.HasGame (HasGame)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Investigators qualified as Investigators
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.I18n
import Arkham.Investigator.Types (Field (..), InvestigatorForm (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Token qualified as Token

newtype DuplicitousDrug = DuplicitousDrug AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duplicitousDrug :: AssetCard DuplicitousDrug
duplicitousDrug = asset DuplicitousDrug Cards.duplicitousDrug

-- | The damage and horror that were on the investigator card when it was set aside.
setAsideTokens :: HasGame m => InvestigatorId -> m Token.Tokens
setAsideTokens iid = do
  mDrug <- selectOne $ assetIs Cards.duplicitousDrug <> assetControlledBy iid
  maybe (pure mempty) (fieldMap AssetMeta (toResultDefault mempty)) mDrug

{- | "Search your bonded cards for Your Other Half": it is not in your bonded
cards while it is in play as your form, nor once it has been removed from the
game.
-}
canFindYourOtherHalf :: InvestigatorMatcher
canFindYourOtherHalf =
  InvestigatorWithBondedCard (cardIs Investigators.yourOtherHalf)
    <> not_ (investigatorIs Investigators.yourOtherHalf)

instance HasAbilities DuplicitousDrug where
  getAbilities (DuplicitousDrug a) =
    scenarioI18n
      [ withI18nTooltip "duplicitousDrug.transform"
          $ restricted a 1 (ControlsThis <> youExist canFindYourOtherHalf)
          $ FastAbility (exhaust a <> assetUseCost a #dose 1)
      , withI18nTooltip "duplicitousDrug.hurt"
          $ restricted a 2 (ControlsThis <> DuringTurn You)
          $ FastAbility Free
      ]

instance RunMessage DuplicitousDrug where
  runMessage msg a@(DuplicitousDrug attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mCard <-
        fieldMap
          InvestigatorBondedCards
          (find (`cardMatch` cardIs Investigators.yourOtherHalf))
          iid
      case mCard of
        Nothing -> pure a
        Just card -> do
          tokens <- field InvestigatorTokens iid
          let wounds = foldr (\t -> Token.setTokens t (Token.countTokens t tokens)) mempty [Token.Damage, Token.Horror]
          -- the damage and horror travel with the card being set aside, so they
          -- come off the investigator rather than being healed
          for_ (mapToList wounds) $ uncurry (removeTokens (attrs.ability 1) iid)
          push $ SetInvestigatorForm iid $ TransfiguredForm card.cardCode
          pure . DuplicitousDrug $ attrs & setMeta wounds
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      chooseOneM iid $ scenarioI18n $ scope "duplicitousDrug" do
        labeled "takeDirectDamage" $ directDamageAndHorror iid source 1 0
        labeled "takeDirectHorror" $ directDamageAndHorror iid source 0 1
      pure a
    _ -> DuplicitousDrug <$> liftRunMessage msg attrs
