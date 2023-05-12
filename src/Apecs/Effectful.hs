{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Apecs.Effectful
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Adaptation of the apecs library for the effectful ecosystem.
-----------------------------------------------------------------------------
module Apecs.Effectful
  ( -- * Effectful Adaptation
    ECS
  , runECS
  , runGC
  , Get
  , Set
  , Destroy
  , Members
  , newEntity
  , newEntity_
  , get
  , tryGet
  , set
  , ($=)
  , destroy
  , exists
  , modify
  , ($~)
  , cmap
  , cmapM
  , cmapM_
  , cfold
  , cfoldM
  , cfoldM_
    -- * Re-exports
  , Entity(..)
  , EntityCounter
  , Not(..)
  , Component(..)
  , Has(..)
  , Cache
  , Global
  , Map
  , Unique
  , SystemT(SystemT)
  , makeWorld
  , makeWorldAndComponents
  , global
  , explInit
  , asks
  ) where

-- apecs
import Apecs      qualified as Apecs
import Apecs.Core qualified as Apecs
import Apecs hiding
  ( Destroy
  , Get
  , Members
  , Set
  , cfold
  , cfoldM
  , cfoldM_
  , cmap
  , cmapM
  , cmapM_
  , exists
  , destroy
  , get
  , modify
  , newEntity
  , newEntity_
  , runGC
  , set
  , ($=)
  , ($~)
  )

-- base
import Data.Kind (Type)

-- effectful-core
import Effectful                 (Eff, Dispatch(Static), DispatchOf, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects(..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)

-- vector
import Data.Vector.Unboxed qualified as U

-- | Provide the ability to query and manipulate worlds of type @w@.
data ECS (w :: Type) :: Effect

type instance DispatchOf (ECS w) = Static WithSideEffects

newtype instance StaticRep (ECS w) = ECS w

-- | Indicates that world @w@ has writeable components of type @c@.
type Set w c = Apecs.Set w IO c

-- | Indicates that world @w@ has readable components of type @c@.
type Get w c = Apecs.Get w IO c

-- | Indicates that world @w@ has deletable components of type @c@.
type Destroy w c = Apecs.Destroy w IO c

-- | Indicates that world @w@ contains components of type @c@.
type Members w c = Apecs.Members w IO c

-- | Run the t'ECS' effect using the world initialization function provided by
-- 'makeWorld' or 'makeWorldAndComponents'.
runECS :: IOE :> es => IO w -> Eff (ECS w : es) a -> Eff es a
runECS worldInit m = do
  w <- unsafeEff_ worldInit
  evalStaticRep (ECS w) m

toEff :: ECS w :> es => System w a -> Eff es a
toEff system = do
  ECS w <- getStaticRep
  unsafeEff_ $ runSystem system w
{-# INLINE toEff #-}

-- | Explicitly invoke the garbage collector.
runGC :: forall w es. ECS w :> es => Eff es ()
runGC = toEff @w Apecs.runGC
{-# INLINE runGC #-}

-- | Writes the given components to a new entity.
newEntity :: forall w c es. (ECS w :> es, Set w c, Get w EntityCounter) => c -> Eff es Entity
newEntity = toEff @w . Apecs.newEntity
{-# INLINE newEntity #-}

-- | Writes the given components to a new entity.
newEntity_ :: forall w c es. (ECS w :> es, Set w c, Get w EntityCounter) => c -> Eff es ()
newEntity_ = toEff @w . Apecs.newEntity_
{-# INLINE newEntity_ #-}

-- | Read a component from an entity.
get :: forall w c es. (ECS w :> es, Get w c) => Entity -> Eff es c
get = toEff @w . Apecs.get
{-# INLINE get #-}

-- | Read a component from an entity, if available.
tryGet :: forall w c es. (ECS w :> es, Get w c) => Entity -> Eff es (Maybe c)
tryGet entity = do
  existing <- exists @c @w entity
  if existing then do
    c <- get @w entity
    pure (Just c)
  else
    pure Nothing
{-# INLINE tryGet #-}

-- | Writes a component to a given entity.
set :: forall w c es. (ECS w :> es, Set w c) => Entity -> c -> Eff es ()
set entity = toEff @w . Apecs.set entity
{-# INLINE set #-}

-- | Writes a component to a given entity.
infixr 2 $=
($=) :: forall w c es. (ECS w :> es, Set w c) => Entity -> c -> Eff es ()
($=) entity = toEff @w . (Apecs.$=) entity
{-# INLINE ($=) #-}

-- | Destroys component @c@ for the given entity.
destroy :: forall c w es. (ECS w :> es, Destroy w c) => Entity -> Eff es ()
destroy entity = toEff @w $ Apecs.destroy entity (Proxy :: Proxy c)
{-# INLINE destroy #-}

-- | Returns whether the given entity has component @c@.
exists :: forall c w es. (ECS w :> es, Get w c) => Entity -> Eff es Bool
exists entity = toEff @w $ Apecs.exists entity (Proxy :: Proxy c)
{-# INLINE exists #-}

-- | Read a component and writes a new component of an entity.
modify :: forall w cx cy es. (ECS w :> es, Get w cx, Set w cy) => Entity -> (cx -> cy) -> Eff es ()
modify entity = toEff @w . Apecs.modify entity
{-# INLINE modify #-}

-- | Read the component @cx@ and writes the component @cy@ of an entity.
infixr 2 $~
($~) :: forall w cx cy es. (ECS w :> es, Get w cx, Set w cy) => Entity -> (cx -> cy) -> Eff es ()
($~) entity = toEff @w . (Apecs.$~) entity
{-# INLINE ($~) #-}

-- | Read the component @cx@ and writes the component @cy@ of all entities.
cmap :: forall w cx cy es. (ECS w :> es, Get w cx, Members w cx, Set w cy) => (cx -> cy) -> Eff es ()
cmap = toEff @w . Apecs.cmap
{-# INLINE cmap #-}

-- | Monadic variant of 'cmap'.
cmapM :: forall w cx cy es. (ECS w :> es, Get w cx, Set w cy, Members w cx) => (cx -> Eff es cy) -> Eff es ()
cmapM f = do
  sx <- toEff @w (Apecs.getStore @w @IO @cx)
  sy <- toEff @w (Apecs.getStore @w @IO @cy)
  sl <- unsafeEff_ $ Apecs.explMembers sx
  U.forM_ sl $ \e -> do
    x <- unsafeEff_ $ Apecs.explGet sx e
    y <- f x
    unsafeEff_ $ Apecs.explSet sy e y
{-# INLINE cmapM #-}

-- | Monadic variant of 'cmap', ignoring the result of the applied function.
cmapM_ :: forall w c es. (ECS w :> es, Get w c, Members w c) => (c -> Eff es ()) -> Eff es ()
cmapM_ f = do
  s <- toEff @w (Apecs.getStore @w @IO @c)
  l <- unsafeEff_ $ Apecs.explMembers s
  U.forM_ l $ \e ->
    unsafeEff_ (Apecs.explGet s e) >>= f
{-# INLINE cmapM_ #-}

-- | Fold over the components @c@ of the game world.
cfold :: forall w c a es. (ECS w :> es, Members w c, Get w c) => (a -> c -> a) -> a -> Eff es a
cfold f = toEff @w . Apecs.cfold f
{-# INLINE cfold #-}

-- | Monadic variant of 'cfold'.
cfoldM :: forall w c a es. (ECS w :> es, Members w c, Get w c) => (a -> c -> Eff es a) -> a -> Eff es a
cfoldM f start = do
  s <- toEff @w (Apecs.getStore @w @IO @c)
  l <- unsafeEff_ $ Apecs.explMembers s
  U.foldM' (\a e -> unsafeEff_ (Apecs.explGet s e) >>= f a) start l
{-# INLINE cfoldM #-}

-- | Monadic variant of 'cfold', ignoring the result.
cfoldM_ :: forall w c a es. (ECS w :> es, Members w c, Get w c) => (a -> c -> Eff es a) -> a -> Eff es ()
cfoldM_ f start = do
  s <- toEff @w (Apecs.getStore @w @IO @c)
  l <- unsafeEff_ $ Apecs.explMembers s
  U.foldM'_ (\a e -> unsafeEff_ (Apecs.explGet s e) >>= f a) start l
{-# INLINE cfoldM_ #-}