module Arkham.Homebrew.TheSymphonyOfErichZann.Assets.ThePiano (thePiano) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits qualified as T
import Arkham.Matcher

newtype ThePiano = ThePiano AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePiano :: AssetCard ThePiano
thePiano = asset ThePiano Cards.thePiano

instance HasModifiersFor ThePiano where
  getModifiersFor (ThePiano a) = modifySelf a []

instance HasAbilities ThePiano where
  {- Only Isabel La Fratta can finish the piece, only while a Piano treachery is
  in play, and only after four differently-typed actions at this location. -}
  getAbilities (ThePiano a) =
    [ restricted
        a
        1
        ( exists (TreacheryWithTrait T.Piano <> InPlayTreachery)
            <> youExist (InvestigatorWithTitle "Isabel La Fratta" <> at_ (locationWithAsset a.id))
        )
        $ parleyAction_
    ]

instance RunMessage ThePiano where
  runMessage msg a@(ThePiano attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs Stories.thePianosMuse
      pure a
    _ -> ThePiano <$> liftRunMessage msg attrs
