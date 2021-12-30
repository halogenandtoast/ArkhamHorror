module Arkham.Treachery.Cards.PushedIntoTheBeyond
  ( PushedIntoTheBeyond(..)
  , pushedIntoTheBeyond
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype PushedIntoTheBeyond = PushedIntoTheBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyond :: TreacheryCard PushedIntoTheBeyond
pushedIntoTheBeyond = treachery PushedIntoTheBeyond Cards.pushedIntoTheBeyond

instance TreacheryRunner env => RunMessage env PushedIntoTheBeyond where
  runMessage msg t@(PushedIntoTheBeyond attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      validAssets <- selectList (AssetOwnedBy You <> AssetNonStory)
      targets <- traverse (traverseToSnd getId) validAssets
      t <$ when
        (notNull targets)
        (push $ chooseOne
          iid
          [ TargetLabel
              (AssetTarget aid)
              [ ShuffleIntoDeck iid (AssetTarget aid)
              , CreateEffect
                (CardCode "02100")
                (Just (EffectCardCode cardCode))
                (toSource attrs)
                (InvestigatorTarget iid)
              ]
          | (aid, cardCode) <- targets
          ]
        )
    _ -> PushedIntoTheBeyond <$> runMessage msg attrs
