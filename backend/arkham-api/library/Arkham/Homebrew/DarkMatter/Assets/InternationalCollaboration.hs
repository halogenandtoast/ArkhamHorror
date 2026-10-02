module Arkham.Homebrew.DarkMatter.Assets.InternationalCollaboration (internationalCollaboration) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Draw.Types (cardDrawAmount, cardDrawSource)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n)
import Arkham.Investigator.Types (Field (InvestigatorDrawing))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype InternationalCollaboration = InternationalCollaboration AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

internationalCollaboration :: AssetCard InternationalCollaboration
internationalCollaboration = asset InternationalCollaboration Cards.internationalCollaboration

playerCardEffect :: SourceMatcher
playerCardEffect = SourceIsPlayerCard <> SourceIsCardEffect

{- | "[reaction] When you would gain resources or draw cards from a player card
effect, exhaust International Collaboration: Choose any investigator. That
investigator may gain that many resources or draw that many cards instead."
-}
instance HasAbilities InternationalCollaboration where
  getAbilities (InternationalCollaboration a) =
    [ controlled_ a 1
        $ triggered
          ( oneOf
              [ GainsResources #when You playerCardEffect (atLeast 1)
              , WouldDrawCardFrom #when You (DeckOf You) playerCardEffect
              ]
          )
          (exhaust a)
    ]

getGainedResources :: [Window] -> Maybe Int
getGainedResources = \case
  [] -> Nothing
  ((windowType -> Window.GainsResources _ _ n) : _) -> Just n
  (_ : ws) -> getGainedResources ws

instance RunMessage InternationalCollaboration where
  runMessage msg a@(InternationalCollaboration attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getGainedResources -> mResources) _ -> do
      investigators <- select Anyone
      for_ mResources \n -> do
        matchingDon't \case
          Do (TakeResources iid' _ _ False) -> iid' == iid
          _ -> False
        chooseTargetM iid investigators \iid' -> chooseOneM iid' $ campaignI18n do
          labeled "internationalCollaboration.gainResources"
            $ gainResources iid' (attrs.ability 1) n
          labeled "internationalCollaboration.decline" nothing

      when (isNothing mResources) do
        mDrawing <- field InvestigatorDrawing iid
        for_ mDrawing \drawing -> do
          -- the window gated on this, but a sibling reaction in the same window
          -- can still ReplaceCurrentCardDraw out from under us
          whenM (sourceMatches (cardDrawSource drawing) playerCardEffect) do
            chooseTargetM iid investigators \iid' -> do
              msgs <- capture $ drawCards iid' (attrs.ability 1) (cardDrawAmount drawing)
              chooseOneM iid' $ campaignI18n do
                labeled "internationalCollaboration.drawCards"
                  $ push (Instead (DoDrawCards iid) (Run msgs))
                labeled "internationalCollaboration.decline"
                  $ push (Instead (DoDrawCards iid) (Run []))
      pure a
    _ -> InternationalCollaboration <$> liftRunMessage msg attrs
