{- | "Revelation - Attach Festering Evil to Henry Jekyll or Edward Hyde."

Jekyll and Hyde are two faces of one card, so only one of them is ever in play
and the revelation has a single legal target.

"When the attached card flips" reads the window both 'flipToHyde' and
'flipToJekyll' raise. Hyde is already removed when the enemy-to-asset half
raises it, hence 'IncludeOutOfPlayEnemy'.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Treacheries.FesteringEvil (
  festeringEvil,
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype FesteringEvil = FesteringEvil TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

festeringEvil :: TreacheryCard FesteringEvil
festeringEvil = treachery FesteringEvil Cards.festeringEvil

-- | The attached card is Jekyll or Hyde, so its location is wherever that face is.
attachedLocation :: TreacheryAttrs -> Maybe LocationMatcher
attachedLocation a =
  (locationWithAsset <$> a.attached.asset) <|> (locationWithEnemy <$> a.attached.enemy)

{- | "Investigators at your location may place up to 2 clues they control on
their location to reduce the cost of this ability by one action (to a minimum of
{fast})": one action per clue, so two clues make the ability free.

The clue count has to be chosen before the action cost is paid -- the actions owed
depend on it -- hence a branch per count rather than one "up to 2" prompt. The
clues are a 'GroupPlaceClueOnLocationCost', so any investigator at the location
can contribute them, each from their own pool onto their own location.
-}
discardCosts :: HasI18n => Cost
discardCosts =
  OrCost
    $ ActionCost 2
    : [ LabeledCost
          (scope "festeringEvil" $ countVar n $ "$" <> labelKey "cluesToPlace")
          (ActionCost (2 - n) <> GroupPlaceClueOnLocationCost (Static n) YourLocation)
      | n <- [1, 2]
      ]

instance HasAbilities FesteringEvil where
  getAbilities (FesteringEvil a) = case attachedLocation a of
    Nothing -> []
    Just _ ->
      scenarioI18n
        [ withI18nTooltip "festeringEvil.flip"
            $ mkAbility a 1
            $ forced
            $ EnemyFlipped #after (IncludeOutOfPlayEnemy edwardHyde)
        , withI18nTooltip "festeringEvil.discard"
            $ restricted a 2 OnSameLocation
            $ ActionAbility mempty Nothing discardCosts
        ]

instance RunMessage FesteringEvil where
  runMessage msg t@(FesteringEvil attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      jekyll <- selectOne henryJekyll
      hyde <- selectOne edwardHyde
      -- Left unattached the engine discards it after the revelation, which is
      -- all that is left if neither face is in play.
      case (jekyll, hyde) of
        (Just aid, _) -> attachTreachery attrs aid
        (_, Just eid) -> attachTreachery attrs eid
        _ -> pure ()
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ (attachedLocation attrs) \lm ->
        selectEach (investigatorAt lm) \iid -> chooseAndDiscardAsset iid (attrs.ability 1)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> FesteringEvil <$> liftRunMessage msg attrs
